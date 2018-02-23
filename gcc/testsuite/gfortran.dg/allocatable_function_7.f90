! { dg-do run }
!
! PR fortran/56138
!
! Contributed by Dominique d'Humieres and John Chludzinski,
! using the code of John Reid
!
implicit none
interface
PURE FUNCTION s_to_c(string)
  CHARACTER(LEN=*),INTENT(IN)   :: string
  CHARACTER(LEN=:),ALLOCATABLE :: s_to_c
ENDFUNCTION s_to_c
end interface
CHARACTER(LEN=:),ALLOCATABLE :: str 
if (s_to_c("ABCdef") /= "ABCdef" .or. len(s_to_c("ABCdef")) /= 6) STOP 1
str = s_to_c("ABCdef")
if (str /= "ABCdef" .or. len(str) /= 6) STOP 2
str(1:3) = s_to_c("123")
if (str /= "123def" .or. len(str) /= 6) STOP 3

end

PURE FUNCTION s_to_c(string) 
  CHARACTER(LEN=*),INTENT(IN)   :: string 
  CHARACTER(LEN=:),ALLOCATABLE :: s_to_c 
  s_to_c = string
END FUNCTION s_to_c 
