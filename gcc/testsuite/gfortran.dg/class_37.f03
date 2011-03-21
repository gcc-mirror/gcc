! { dg-do compile }
! Test fix for PR47082, in which an ICE in the ALLOCATE at line 248.
!
! Contributed by Salvatore Filippone  <salvatore.filippone@uniroma2.it>
!
module psb_penv_mod

  interface psb_init
    module procedure  psb_init
  end interface

  interface psb_exit
    module procedure  psb_exit
  end interface

  interface psb_info
    module procedure psb_info
  end interface

  integer, private, save :: nctxt=0



contains


  subroutine psb_init(ictxt,np,basectxt,ids)
    implicit none 
    integer, intent(out) :: ictxt
    integer, intent(in), optional :: np, basectxt, ids(:)


    ictxt = nctxt
    nctxt = nctxt + 1

  end subroutine psb_init

  subroutine psb_exit(ictxt,close)
    implicit none 
    integer, intent(inout) :: ictxt
    logical, intent(in), optional :: close

    nctxt = max(0, nctxt - 1)    

  end subroutine psb_exit


  subroutine psb_info(ictxt,iam,np)

    implicit none 

    integer, intent(in)  :: ictxt
    integer, intent(out) :: iam, np

    iam = 0
    np  = 1

  end subroutine psb_info


end module psb_penv_mod


module psb_indx_map_mod

  type      :: psb_indx_map

    integer :: state          = -1
    integer :: ictxt          = -1
    integer :: mpic           = -1
    integer :: global_rows    = -1
    integer :: global_cols    = -1
    integer :: local_rows     = -1
    integer :: local_cols     = -1


  end type psb_indx_map

end module psb_indx_map_mod



module psb_gen_block_map_mod
  use psb_indx_map_mod
  
  type, extends(psb_indx_map) :: psb_gen_block_map
    integer :: min_glob_row   = -1
    integer :: max_glob_row   = -1
    integer, allocatable :: loc_to_glob(:), srt_l2g(:,:), vnl(:)
  contains

    procedure, pass(idxmap)  :: gen_block_map_init => block_init

  end type psb_gen_block_map

  private ::  block_init

contains

  subroutine block_init(idxmap,ictxt,nl,info)
    use psb_penv_mod
    implicit none 
    class(psb_gen_block_map), intent(inout) :: idxmap
    integer, intent(in)  :: ictxt, nl
    integer, intent(out) :: info
    !  To be implemented
    integer :: iam, np, i, j, ntot
    integer, allocatable :: vnl(:)

    info = 0
    call psb_info(ictxt,iam,np) 
    if (np < 0) then 
      info = -1
      return
    end if
    
    allocate(vnl(0:np),stat=info)
    if (info /= 0)  then
      info = -2
      return
    end if
    
    vnl(:)   = 0
    vnl(iam) = nl
    ntot = sum(vnl)
    vnl(1:np) = vnl(0:np-1)
    vnl(0) = 0
    do i=1,np
      vnl(i) = vnl(i) + vnl(i-1)
    end do
    if (ntot /= vnl(np)) then 
! !$      write(0,*) ' Mismatch in block_init ',ntot,vnl(np)
    end if
    
    idxmap%global_rows  = ntot
    idxmap%global_cols  = ntot
    idxmap%local_rows   = nl
    idxmap%local_cols   = nl
    idxmap%ictxt        = ictxt
    idxmap%state        = 1

    idxmap%min_glob_row = vnl(iam)+1
    idxmap%max_glob_row = vnl(iam+1) 
    call move_alloc(vnl,idxmap%vnl)
    allocate(idxmap%loc_to_glob(nl),stat=info) 
    if (info /= 0)  then
      info = -2
      return
    end if
    
  end subroutine block_init

end module psb_gen_block_map_mod


module psb_descriptor_type
  use psb_indx_map_mod

  implicit none


  type psb_desc_type
    integer, allocatable  :: matrix_data(:)
    integer, allocatable  :: halo_index(:)
    integer, allocatable  :: ext_index(:)
    integer, allocatable  :: ovrlap_index(:)
    integer, allocatable  :: ovrlap_elem(:,:)
    integer, allocatable  :: ovr_mst_idx(:)
    integer, allocatable  :: bnd_elem(:)
    class(psb_indx_map), allocatable :: indxmap
    integer, allocatable  :: lprm(:)
    type(psb_desc_type), pointer     :: base_desc => null()
    integer, allocatable  :: idx_space(:)
  end type psb_desc_type


end module psb_descriptor_type

module psb_cd_if_tools_mod

  use psb_descriptor_type
  use psb_gen_block_map_mod

  interface psb_cdcpy
    subroutine psb_cdcpy(desc_in, desc_out, info)
      use psb_descriptor_type

      implicit none
      !....parameters...

      type(psb_desc_type), intent(in)  :: desc_in
      type(psb_desc_type), intent(out) :: desc_out
      integer, intent(out)             :: info
    end subroutine psb_cdcpy
  end interface


end module psb_cd_if_tools_mod

module psb_cd_tools_mod

  use psb_cd_if_tools_mod

  interface psb_cdall

    subroutine psb_cdall(ictxt, desc, info,mg,ng,vg,vl,flag,nl,repl, globalcheck)
      use psb_descriptor_type
      implicit None
      Integer, intent(in)               :: mg,ng,ictxt, vg(:), vl(:),nl
      integer, intent(in)               :: flag
      logical, intent(in)               :: repl, globalcheck
      integer, intent(out)              :: info
      type(psb_desc_type), intent(out)  :: desc
      
      optional :: mg,ng,vg,vl,flag,nl,repl, globalcheck
    end subroutine psb_cdall
   
  end interface

end module psb_cd_tools_mod
module psb_base_tools_mod
  use psb_cd_tools_mod
end module psb_base_tools_mod

subroutine psb_cdall(ictxt, desc, info,mg,ng,vg,vl,flag,nl,repl, globalcheck)
  use psb_descriptor_type
  use psb_gen_block_map_mod
  use psb_base_tools_mod, psb_protect_name => psb_cdall
  implicit None
  Integer, intent(in)               :: mg,ng,ictxt, vg(:), vl(:),nl
  integer, intent(in)               :: flag
  logical, intent(in)               :: repl, globalcheck
  integer, intent(out)              :: info
  type(psb_desc_type), intent(out)  :: desc

  optional :: mg,ng,vg,vl,flag,nl,repl, globalcheck
  integer :: err_act, n_, flag_, i, me, np, nlp, nnv, lr
  integer, allocatable :: itmpsz(:) 



  info = 0
  desc%base_desc => null() 
  if (allocated(desc%indxmap)) then 
    write(0,*) 'Allocated on an intent(OUT) var?'
  end if

  allocate(psb_gen_block_map :: desc%indxmap, stat=info)
  if (info == 0) then 
    select type(aa => desc%indxmap) 
    type is (psb_gen_block_map) 
      call aa%gen_block_map_init(ictxt,nl,info)
    class default 
        ! This cannot happen 
      info = -1
    end select
  end if

  return

end subroutine psb_cdall

 
