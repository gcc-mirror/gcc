! { dg-do compile }
! Tests the patch for PRs 25084, 20852, 25085 and 25086, all of
! which involve assumed character length functions.
! This test checks the things that should not emit errors.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
function is_OK (ch)                ! { dg-warning "Obsolescent feature" }
  character(*) is_OK, ch           ! OK in an external function
  is_OK = ch
end function is_OK

! The warning occurs twice for the next line; for 'more_OK' and for 'fcn';
function more_OK (ch, fcn)         ! { dg-warning "Obsolescent feature" }
  character(*) more_OK, ch
  character (*), external :: fcn   ! OK as a dummy argument
  more_OK = fcn (ch)
end function more_OK

  character(4) :: answer
  character(4), external :: is_OK, more_OK

  answer = is_OK ("isOK")          ! LEN defined in calling scope
  print *, answer

  answer = more_OK ("okay", is_OK) ! Actual arg has defined LEN
  print *, answer

  answer = also_OK ("OKOK")
  print *, answer

contains
  function also_OK (ch)
    character(4) also_OK
    character(*) ch
    also_OK = is_OK (ch)            ! LEN obtained by host association
  end function also_OK
END

