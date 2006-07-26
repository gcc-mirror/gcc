! { dg-do run }
! { dg-shouldfail "UNIT is not open before CLOSE" }
! PR28335 Check for error on no unit.
  close(88) ! { dg-output "Can't find specified UNIT in CLOSE" }
  end


