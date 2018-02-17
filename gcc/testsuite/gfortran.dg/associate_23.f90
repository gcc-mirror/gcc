! { dg-do run }
!
! Tests the fix for PR64933
!
! Contributed by Olivier Marsden  <olivier.marsden@ecmwf.int>
!
program test_this
  implicit none
  character(len = 15) :: char_var, char_var_dim (3)
  character(len = 80) :: buffer

! Original failing case reported in PR
  ASSOCIATE(should_work=>char_var)
    should_work = "test succesful"
    write (buffer, *) should_work(5:14)
  END ASSOCIATE

  if (trim (buffer) .ne. "  succesful") STOP 1

! Found to be failing during debugging
  ASSOCIATE(should_work=>char_var_dim)
    should_work = ["test SUCCESFUL", "test_SUCCESFUL", "test.SUCCESFUL"]
    write (buffer, *) should_work(:)(5:14)
  END ASSOCIATE

  if (trim (buffer) .ne. "  SUCCESFUL_SUCCESFUL.SUCCESFUL") STOP 2

! Found to be failing during debugging
  ASSOCIATE(should_work=>char_var_dim(1:2))
    should_work = ["test SUCCESFUL", "test_SUCCESFUL"]
    write (buffer, *) should_work(:)(5:14)
  END ASSOCIATE

  if (trim (buffer) .ne. "  SUCCESFUL_SUCCESFUL") STOP 3

end program
