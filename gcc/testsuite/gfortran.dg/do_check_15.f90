! { dg-do compile }
! PR fortran/96556 - this used to cause an ICE.
! Test case by Juergen Reuter.
module polarizations

  implicit none
  private

  type :: smatrix_t
     private
     integer :: dim = 0
     integer :: n_entry = 0
     integer, dimension(:,:), allocatable :: index
   contains
     procedure :: write => smatrix_write
  end type smatrix_t

  type, extends (smatrix_t) :: pmatrix_t
     private
   contains
     procedure :: write => pmatrix_write
     procedure :: normalize => pmatrix_normalize
  end type pmatrix_t

contains

  subroutine msg_error (string)
    character(len=*), intent(in), optional :: string
  end subroutine msg_error
  
  subroutine smatrix_write (object)
    class(smatrix_t), intent(in) :: object
  end subroutine smatrix_write

  subroutine pmatrix_write (object)
    class(pmatrix_t), intent(in) :: object
    call object%smatrix_t%write ()
  end subroutine pmatrix_write

  subroutine pmatrix_normalize (pmatrix)
    class(pmatrix_t), intent(inout) :: pmatrix
    integer :: i, hmax
    logical :: fermion, ok
    do i = 1, pmatrix%n_entry
       associate (index => pmatrix%index(:,i))
         if (index(1) == index(2)) then
            call error ("diagonal must be real")
         end if
       end associate
    end do
  contains
    subroutine error (msg)
      character(*), intent(in) :: msg
      call pmatrix%write ()
    end subroutine error
  end subroutine pmatrix_normalize

end module polarizations
