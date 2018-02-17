! { dg-do run }
!
! Test the fix for PR79072 comment #12. A description of the problem
! is to be found in class_result_5.f90.
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
  character(3), target :: a = 'foo'
  class(*), pointer :: b
  b => ptr()
  select type (b)
    type is (character(*))
      if (a .ne. "bar") STOP 1
  end select
contains
  function ptr()
    class(*), pointer :: ptr
    ptr => a
    select type (ptr)
      type is (character(*))
        ptr = "bar"
    end select
  end function
end
