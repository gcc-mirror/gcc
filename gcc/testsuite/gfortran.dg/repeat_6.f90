! { dg-do run }
!
! PR34559 -- ICE on empty string literals
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!

  character(len=200) :: string = "a" // repeat ("", 3)       &
                                     // repeat ("xxx", 0)    &
                                     // repeat ("string", 2)

  if (string /= "astringstring") CALL abort()
end
