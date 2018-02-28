! { dg-do run }

program main
  if (bug() /= "abcdefghij") STOP 1
contains
  function bug()
    character(len=10) :: bug
    character(len=1), dimension(:), pointer :: p_chars
    allocate(p_chars(10))
    p_chars = ['a','b','c','d','e','f','g','h','i','j']
    forall (i=1:len(bug))
      bug(i:i) = p_chars(i)
    end forall
    deallocate(p_chars)
  end function bug
end program main
