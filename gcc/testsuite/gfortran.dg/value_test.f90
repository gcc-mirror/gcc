! { dg-do run }
program valueTests
  integer :: myInt
  interface
     subroutine mySub(myInt)
       integer, value :: myInt
     end subroutine mySub
  end interface

  myInt = 10

  call mySub(myInt)
  ! myInt should be unchanged since pass-by-value
  if(myInt .ne. 10) then
     call abort ()
  endif
end program valueTests

subroutine mySub(myInt)
  integer, value :: myInt
  myInt = 11
end subroutine mySub
  
