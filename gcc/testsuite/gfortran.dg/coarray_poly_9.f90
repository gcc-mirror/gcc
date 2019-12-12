! { dg-do run }
! { dg-options "-fcoarray=single" }
!
! Test the fix for PR91726.
!
! Contributed by Gerhardt Steinmetz  <gscfq@t-online.de>
!
module m
   type s
      class(*), allocatable :: a[:]    ! This ICEd
   end type
   type t
      class(*), allocatable :: a(:)[:] ! This was OK
   end type
end

  use m
  call foo
  call bar
contains
  subroutine foo
    type (s) :: a
    integer(4) :: i = 42_4
    allocate (a%a[*], source = i)     ! This caused runtime segfaults
    select type (z => a%a)            ! ditto
      type is (integer(4))
      if (z .ne. 42_4) stop 1
    end select
  end subroutine
  subroutine bar                      ! Arrays always worked
    type (t) :: a
    allocate (a%a(3)[*], source = [1_4, 2_4, 3_4])
    select type (z => a%a)
      type is (integer(4))
      if (any (z .ne. [1_4, 2_4, 3_4])) stop 2
    end select
  end subroutine
end
