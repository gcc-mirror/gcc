! { dg-do compile }
! { dg-options "-fcoarray=single -g" }
!
! Test the fix for PR96737 in which the 'TYPE_CANONICAL' was not campatible
! in the submodule.
!
! Contributed by Andre Vehreschild  <vehre@gcc.gnu.org>
!
module surface_packages
  implicit none

  type flux_planes
    integer, allocatable :: normals(:,:)
  end type

  type package
    integer id
    type(flux_planes), allocatable :: surface_fluxes(:)
    integer, allocatable :: positions(:,:,:,:)
  end type

  type surfaces
    type(package), allocatable :: halo_outbox(:,:,:)
  contains
    procedure, nopass :: set_halo_outbox
    procedure, nopass :: get_surface_normal_spacing
  end type

  type problem_discretization
    type(surfaces) block_surfaces
  end type

  interface
    module subroutine set_halo_outbox(my_halo_outbox)
      implicit none
      type(package), intent(in) :: my_halo_outbox(:,:,:)
    end subroutine

    module subroutine get_surface_normal_spacing
    end subroutine
  end interface

end module

submodule(surface_packages) implementation
  implicit none
  type(surfaces), save :: singleton[*]
contains

  module procedure get_surface_normal_spacing
    integer i, b, d, f

    do i=1,num_images()
      associate( positions => reshape(i*[5,4,3,2], [2,1,1,2]), normals => reshape(i*[6,6,6], [3,1]) )
        do b=1,size(singleton[i]%halo_outbox,1)
          do d=1,size(singleton[i]%halo_outbox,2)
            do f=1,size(singleton[i]%halo_outbox,3)
              if ( .not. all([singleton[i]%halo_outbox(b,d,f)%positions == positions]) ) error stop "positions"
              if ( .not. all([singleton[i]%halo_outbox(b,d,f)%surface_fluxes(1)%normals == normals] ) )  error stop "normals"
            end do
          end do
        end do
      end associate
    end do
  end procedure

  module procedure set_halo_outbox
    singleton%halo_outbox = my_halo_outbox
    sync all
  end procedure

end submodule

program main
  use surface_packages, only : problem_discretization, package
  implicit none
  type(problem_discretization) global_grid
  type(package), allocatable :: bare(:,:,:)
  integer i, j, k

  associate( me=>this_image() )

    allocate( bare(me,3,2) )

    do i=1, size(bare,1)
      bare(i,:,:)%id = i
      do j=1, size(bare,2)
        do k=1, size(bare,3)
          bare(i,j,k)%positions =  reshape(me*[5,4,3,2], [2,1,1,2])
          allocate( bare(i,j,k)%surface_fluxes(1) )
          bare(i,j,k)%surface_fluxes(1)%normals = reshape(me*[6,6,6], [3,1])
        end do
      end do
    end do

    call global_grid%block_surfaces%set_halo_outbox(bare)
    call global_grid%block_surfaces%get_surface_normal_spacing

  end associate

  sync all
  if (this_image()==1) print *,"Test passed"
end program main
