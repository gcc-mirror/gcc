! { dg-do run }
! { dg-options "-fno-tree-vrp" }
! PR fortran/89282
! Contributed by Federico Perini.
!
module myclass
    use iso_fortran_env, only: real64
    implicit none

    ! My generic type
    type :: t

        integer :: n=0
        real(real64), allocatable :: x(:)

        contains

          procedure :: init => t_init
          procedure :: destroy => t_destroy
          procedure :: print => t_print

          procedure, private, pass(this) :: x_minus_t
          generic :: operator(-) => x_minus_t


    end type t

    contains

    elemental subroutine t_destroy(this)
       class(t), intent(inout) :: this
       this%n=0
       if (allocated(this%x)) deallocate(this%x)
    end subroutine t_destroy

    subroutine t_init(this,n)
      class(t), intent(out) :: this
      integer, intent(in) :: n
      call this%destroy()
      this%n=n
      allocate(this%x(n))
    end subroutine t_init

    type(t) function x_minus_t(x,this) result(xmt)
       real(real64), intent(in) :: x
       class(t), intent(in) :: this
       call xmt%init(this%n)
       xmt%x(:) = x-this%x(:)
    end function x_minus_t

    subroutine t_print(this,msg)
       class(t), intent(in) :: this
       character(*), intent(in) :: msg

       integer :: i

       print "('type(t) object <',a,'>, size=',i0)", msg,this%n
       do i=1,this%n
         print "('  x(',i0,') =',1pe12.5)",i,this%x(i)
       end do

    end subroutine t_print

end module myclass


program test_overloaded
    use myclass
    implicit none

    type(t) :: t1,r1

    ! Error with result (5)  
    call t1%init(5);  t1%x(:) = 1.0_real64; r1 = 3.0_real64 - t1
    if (any(r1%x /= 2.0)) stop 1
!    call r1%print('r1')

    ! No errors
    call t1%init(6);  t1%x(:) = 1.0_real64; r1 = 3.0_real64 - t1
    if (any(r1%x /= 2.0)) stop 2
!    call r1%print('r1')
    return

end program test_overloaded
