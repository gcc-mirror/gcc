! { dg-do run }
! { dg-shouldfail "UNIT is not open before FLUSH" }
! PR28335 Check for error on no unit.
  flush(88) ! { dg-output "Can't find specified UNIT in FLUSH" }
  end


