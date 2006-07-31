! { dg-do run }
! { dg-shouldfail "UNIT does not exist for FLUSH" }
! PR28335 Check for error on no unit.
  close(88)
  flush(88) ! { dg-output "Specified UNIT in FLUSH is not connected" }
  end

