! { dg-do run }
! pr44477 ENDFILE not allowed after ENDFILE
!-------------------------------------------
  open(10, form='formatted', &
    action='write', position='rewind', status="scratch")
  endfile(10)
  endfile(10) ! { dg-shouldfail "Cannot perform ENDFILE" }
end
