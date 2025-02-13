! PR debug/118790
! { dg-do compile }
! { dg-options "-O2 -g -fopenmp --param ggc-min-expand=0 --param ggc-min-heapsize=0" }

module ec_args_mod
  private
  public :: ec_argc, ec_argv, ec_args
  interface
    function ec_argc() bind(c,name="ec_argc") result(argc)
    end function
  end interface
contains
  function ec_argv(iarg) result(argv)
    use, intrinsic :: iso_c_binding
    character(len=:), allocatable :: argv
    type(c_ptr), pointer :: argv_cptrs(:)
    argv = to_string (argv_cptrs(iarg+1), 1024)
  end function
  subroutine ec_args()
    use, intrinsic :: iso_c_binding
    integer :: argc
    type(c_ptr) :: argv(512)
    if (ec_argc() == 0) then
      call read_command_line(argc,argv)
    end if
  end subroutine
  function to_string(cptr,maxlen) result(string)
    use, intrinsic :: iso_c_binding
    character(len=:), allocatable :: string
    type(c_ptr) :: cptr
    character(kind=c_char,len=1), pointer :: s(:)
    call c_f_pointer (cptr, s, (/maxlen/))
    do
      if (s(i) == c_null_char) exit
      i = i + 1
    end do
    nchars = i - 1
    allocate (character(len=(nchars)) :: string)
    do i=1,nchars
      string(i:i) = s(i)
    end do
  end function
  subroutine read_command_line(argc,argv)
    use, intrinsic :: iso_c_binding
    integer, parameter :: cmd_max_len = 1024 * 512
    integer(c_int) :: argc
    type(c_ptr) :: argv(:)
    character(kind=c_char,len=1), save, target :: args(cmd_max_len)
    character(kind=c_char,len=cmd_max_len), save, target :: cmd
    character(kind=c_char,len=cmd_max_len) :: arg
    integer(c_int) :: iarg, arglen, pos, ich, argpos
    do ich=1,len(cmd)
      if (cmd(ich:ich) == " ") then
        cmd(ich:ich) = c_null_char
      end if
    end do
    do iarg=1,argc
      do ich=1,arglen
        args(pos) = arg(ich:ich)
      end do
      args(pos) = c_null_char;  pos = pos+1
      argv(iarg+1) = c_loc(args(argpos))
    end do
  end subroutine
end module
module mpl_mpif
  integer mpi_status_size
end module mpl_mpif
subroutine ec_meminfo(ku,cdstring,kcomm,kbarr,kiotask,kcall)
  use mpl_mpif
  interface
    subroutine ec_pmon(energy,power)
    end subroutine ec_pmon
  end interface
  character(len=*), intent(in) :: cdstring
  integer :: ii,jj,i,j,k,myproc,nproc,len,error,nodenum,jid
  integer :: tasksmall,nodehuge,memfree,cached,nfree
  integer :: nnuma
  integer,dimension(:),allocatable,save :: smallpage,hugepage
  integer :: n18
  integer,dimension(:,:),allocatable,save :: node, bucket
  character(len=256) :: clstr
  character(len=20)  :: nodename,lastnode,clmaxnode
  character(len=160) ::line
  character(len=5+1+len(cdstring)) :: id_string
  integer :: irecv_status(mpi_status_size)
  logical :: llnocomm, llnohdr
  logical, save :: llfirst_time = .true.
  type ranknode_t
    integer :: nodenum
    integer :: pid
    integer :: rank_world
    integer :: rank
    integer :: iorank
    integer :: nodemaster
    integer, allocatable :: coreids(:)
    character(len=len(clstr)) :: str
  end type
  type (ranknode_t), allocatable, save :: rn(:)
  integer, allocatable :: coreids(:)
  character(len=64) :: clpfx
  if (llfirst_time .and. .not. llnocomm) then
    allocate(coreids(0:maxth-1))
    coreids(:) = -1
!$omp parallel num_threads(maxth) shared(coreids) private(i,myth,icoreid)
    do i=1,maxth
      icoreid = ec_coreid()
      myth = omp_get_thread_num()
      coreids(myth) = icoreid
    end do
!$omp end parallel
    if (myproc == 0) then
      call slash_proc
      allocate(rn(0:nproc-1))
      do i=0,nproc-1
        rn(i)%nodenum = -1
        if (i > 0) then
          call mpi_recv(lastnode, len(lastnode), mpi_byte, i, itag, kcomm, irecv_status, error)
          call check_error("from mpi_recv(lastnode)","/tmp/fiat/src/fiat/util/ec_meminfo.f90",258)
          call mpi_comm_rank(mpi_comm_world,k,error)
          rn(i)%rank = 0
          rn(i)%str = cdstring
          rn(i)%pid = ec_getpid()
        end if
        rn(i)%rank_world = k
        rn(i)%iorank = iorank
        rn(i)%nodemaster = 0
        call check_error("from mpi_send(iam_nodemaster)","/tmp/fiat/src/fiat/util/ec_meminfo.f90",305)
      end do
      call mpi_send(clstr,len(clstr),mpi_byte,0,itag+5,kcomm,error)
      call mpi_send(clstr,maxth,mpi_integer4,0,itag+6,kcomm,error)
      call mpi_recv(lastnode,1,mpi_integer4,0,itag+7,kcomm,irecv_status,error)
    end if
  end if
contains
  subroutine slash_proc
    read(line(idx+iclkeylen-1:),*,err=99,end=98) node(:,0)
98  continue
    do k=1,maxnuma-1
      read(line(idx+iclkeylen-1:),*,err=99) node(0:n18-1,k)
    end do
99  continue
    close(502)
    smallpage(:) = 0
    do k=0,nnuma-1
      do j=0,n18-1
      end do
      smallpage(k) = sum(bucket(0:8,k))/onemega
      hugepage(k) = sum(bucket(9:n18-1,k))/onemega
    end do
    open(file="/proc/meminfo",unit=502,status="old",action="read",err=977)
    do i=1,10
      read(502,'(a)',err=988,end=988) line
      if(line(1:7) == "memfree") then
        read(line(9:80),*) memfree
      else if(line(1:6) == "cached") then
        read(line(8:80),*) cached
      end if
    end do
988 continue
    close(502)
977 continue
    memfree=memfree/1024
  end subroutine slash_proc
  subroutine prt_data(kun,knodenum,cdlastnode,kcall)
    character(len=4096) :: clbuf
    write(clbuf(ilen+1:),'(2x,2i8,3x,2f6.1,1x,i9,1x,i6,1x,a7,1x,a)') trim(id_string)
  end subroutine prt_data
  subroutine check_error(clwhat,srcfile,srcline)
    character(len=*), intent(in) :: clwhat, srcfile
    integer, intent(in) :: srcline
    if (error /= 0) then
      write(0,'(a,i0,1x,a,1x,"(",a,":",i0,")")') &
            & clpfx(1:ipfxlen)//"## ec_meminfo error code =",error,clwhat,srcfile,srcline
      call mpi_abort(kcomm,-1,error)
    end if
  end subroutine check_error
  subroutine rnsort(kun)
    do i=0,nproc-1
    end do
  end subroutine rnsort
end subroutine ec_meminfo
