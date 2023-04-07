! { dg-do compile }
! PR fortran/58331 - rank checking for CLASS dummy arguments

module mymod
  implicit none
contains
  subroutine mysub(a, n)
    integer,  intent(in) :: n
    class(*), intent(in) :: a(n)

    select type(a)
    type is(integer)
       print *,'a is integer'
       print *, "n=", n, '  a=', a
    class default
       print *,'a is unsupported type'
       stop 1
    end select
  end

  ! Assumed rank
  subroutine sub_ar (a)
    class(*), intent(in) :: a(..)
    print *, rank (a), size (a), ":", shape (a)
  end

  ! Assumed shape
  subroutine sub_as1 (a)
    class(*), intent(in) :: a(:)
    print *, rank (a), size (a), ":", shape (a)
  end
  subroutine sub_as2 (b)
    class(*), intent(in) :: b(:,:)
    print *, rank (b), size (b), ":", shape (b)
  end
end

program p
  use mymod
  implicit none
  integer :: a(3)   = [11, 12, 13]
  integer :: b(2,2) = reshape([21, 22, 23, 24], [2,2])
  integer :: c      = 1

  call mysub(a,3)
  call mysub(b,4)
  call sub_ar(a)
  call sub_ar(b)
  call sub_ar(c)
  call sub_as1(a)
  call sub_as2(b)
  !
  call mysub(c,1) ! { dg-error "rank-1 and scalar" }
  call sub_as1(b) ! { dg-error "rank-1 and rank-2" }
  call sub_as2(a) ! { dg-error "rank-2 and rank-1" }
end
