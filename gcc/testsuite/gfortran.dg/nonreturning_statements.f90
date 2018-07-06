! { dg-final { scan-assembler-not "should_be_noreturn" } }
! PR 17758
! This checks that non-returning subroutines and statements
! really don't return by calling non-existing subroutines
! afterwards.  These calls are supposed to be optimized away, so
! they won't show up in the generated assembly.
program main
  character(len=5) :: c
  c = '12345'
  read(unit=c,fmt='(A)') i
  select case(i)
     case(1)
        STOP 1
        call abort_should_be_noreturn
     case(2)
        stop 65
        call stop_numeric_should_be_noreturn
     case(3)
        stop "foobar"
        call stop_string_should_be_noreturn
     case(4)
        call exit
        call exit_should_be_noreturn
     end select
end program main
