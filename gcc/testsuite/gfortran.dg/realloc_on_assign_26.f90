! { dg-do run }
! PR 68147 - no temprorary within the IF statement.
! Original test case by Martin Reinecke.
program test
  implicit none
  character(len=:),allocatable ::name
  name="./a.out"
  if (index(name,"/")  /=  0) THEN
    name=name(3:)
    if (name .ne. "a.out") STOP 1
  endif
end program
