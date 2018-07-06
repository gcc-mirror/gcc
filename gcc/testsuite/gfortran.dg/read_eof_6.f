! { dg-do run }
! PR43320 Missing EOF on read from empty file.
      open(8,status='scratch',form='formatted')  ! Create empty file
      read(8,'(a80)', end=123)  ! Reading from an empty file should be an EOF
      STOP 1
123   continue
      end
