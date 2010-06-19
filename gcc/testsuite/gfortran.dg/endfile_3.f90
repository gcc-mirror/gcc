! { dg-do run }
! pr44477 READ/WRITE not allowed after ENDFILE 
!-------------------------------------------
  open(10, form='formatted', &
    action='write', position='rewind', status="scratch")
  endfile(10)
  write(10,'(a)') "aa" ! { dg-shouldfail "Cannot perform ENDFILE" }
end

