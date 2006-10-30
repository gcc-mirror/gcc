! { dg-do compile }
! Check keyword checking for specifiers
! PR fortran/29452
  character(len=20) :: str
  write(13,'(a)',advance='yes')  'Hello:'
  write(13,'(a)',advance='no')   'Hello:'
  write(13,'(a)',advance='y')    'Hello:' ! { dg-error "ADVANCE=specifier at \\(1\\) must have value = YES or NO." }
  write(13,'(a)',advance='yet')  'Hello:' ! { dg-error "ADVANCE=specifier at \\(1\\) must have value = YES or NO." }
  write(13,'(a)',advance='yess') 'Hello:' ! { dg-error "ADVANCE=specifier at \\(1\\) must have value = YES or NO." }
  end
