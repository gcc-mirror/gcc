! { dg-do run }
! Test assignments of nested derived types with character allocatable
! components(PR 20541). Subroutine test_ab6 checks out a bug in a test
! version of gfortran's allocatable arrays.
!
! Contributed by Erik Edelmann  <eedelmann@gcc.gnu.org>
!            and Paul Thomas  <pault@gcc.gnu.org>
!
  type :: a
    character(4), allocatable :: ch(:)
  end type a

  type :: b
    type (a), allocatable :: at(:)
  end type b

  type(a) :: x(2)
  type(b) :: y(2), z(2)

  character(4) :: chr1(4) = (/"abcd","efgh","ijkl","mnop"/)
  character(4) :: chr2(4) = (/"qrst","uvwx","yz12","3456"/)

  x(1) = a(chr1)

 ! Check constructor with character array constructors.
  x(2) = a((/"qrst","uvwx","yz12","3456"/))

  y(1) = b((/x(1),x(2)/))
  y(2) = b((/x(2),x(1)/))

  y(2) = y(1)

  if (any((/((y(2)%at(i)%ch(j),j=1,4),i=1,2)/) .ne. &
          (/chr1, chr2/))) STOP 1

  call test_ab6 ()

contains

  subroutine test_ab6 ()
! This subroutine tests the presence of a scalar derived type, intermediate
! in a chain of derived types with allocatable components.
! Contributed by Salvatore Filippone  <salvatore.filippone@uniroma2.it>

    type b
      type(a)  :: a
    end type b

    type c
      type(b), allocatable :: b(:) 
    end type c

    type(c)    :: p
    type(b)   :: bv

    p = c((/b(a((/"Mary","Lamb"/)))/))
    bv = p%b(1)

    if (any ((bv%a%ch(:)) .ne. (/"Mary","Lamb"/))) STOP 2

end subroutine test_ab6

end
