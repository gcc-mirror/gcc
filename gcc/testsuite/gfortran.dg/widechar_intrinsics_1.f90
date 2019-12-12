! { dg-do compile }
! { dg-options "-fmax-errors=100000" }

  character(kind=1,len=20) :: s1, t1, u1, v1
  character(kind=4,len=20) :: s4, t4, u4, v4

  call date_and_time(date=s1)
  call date_and_time(time=s1)
  call date_and_time(zone=s1)
  call date_and_time(s1, t1, u1)

  call date_and_time(date=s4) ! { dg-error "must be of kind 1" }
  call date_and_time(time=s4) ! { dg-error "must be of kind 1" }
  call date_and_time(zone=s4) ! { dg-error "must be of kind 1" }
  call date_and_time(s4, t4, u4) ! { dg-error "must be of kind 1" }

  call get_command(s1)
  call get_command(s4) ! { dg-error "'CHARACTER\\(20,4\\)' to 'CHARACTER\\(\\*\\)'" }

  call get_command_argument(1, s1)
  call get_command_argument(1, s4) ! { dg-error "'CHARACTER\\(20,4\\)' to 'CHARACTER\\(\\*\\)'" }

  call get_environment_variable("PATH", s1)
  call get_environment_variable(s1)
  call get_environment_variable(s1, t1)
  call get_environment_variable(4_"PATH", s1) ! { dg-error "'CHARACTER\\(4,4\\)' to 'CHARACTER\\(\\*\\)'" }
  call get_environment_variable(s4) ! { dg-error "'CHARACTER\\(20,4\\)' to 'CHARACTER\\(\\*\\)'" }
  call get_environment_variable(s1, t4) ! { dg-error "'CHARACTER\\(20,4\\)' to 'CHARACTER\\(\\*\\)'" }
  call get_environment_variable(s4, t1) ! { dg-error "'CHARACTER\\(20,4\\)' to 'CHARACTER\\(\\*\\)'" }

  print *, lge(s1,t1)
  print *, lge(s1,"foo")
  print *, lge("foo",t1)
  print *, lge("bar","foo")

  print *, lge(s1,t4) ! { dg-error "must be of kind 1" }
  print *, lge(s1,4_"foo") ! { dg-error "must be of kind 1" }
  print *, lge("foo",t4) ! { dg-error "must be of kind 1" }
  print *, lge("bar",4_"foo") ! { dg-error "must be of kind 1" }

  print *, lge(s4,t1) ! { dg-error "must be of kind 1" }
  print *, lge(s4,"foo") ! { dg-error "must be of kind 1" }
  print *, lge(4_"foo",t1) ! { dg-error "must be of kind 1" }
  print *, lge(4_"bar","foo") ! { dg-error "must be of kind 1" }

  print *, lge(s4,t4) ! { dg-error "must be of kind 1" }
  print *, lge(s4,4_"foo") ! { dg-error "must be of kind 1" }
  print *, lge(4_"foo",t4) ! { dg-error "must be of kind 1" }
  print *, lge(4_"bar",4_"foo") ! { dg-error "must be of kind 1" }

  print *, lgt(s1,t1)
  print *, lgt(s1,"foo")
  print *, lgt("foo",t1)
  print *, lgt("bar","foo")

  print *, lgt(s1,t4) ! { dg-error "must be of kind 1" }
  print *, lgt(s1,4_"foo") ! { dg-error "must be of kind 1" }
  print *, lgt("foo",t4) ! { dg-error "must be of kind 1" }
  print *, lgt("bar",4_"foo") ! { dg-error "must be of kind 1" }

  print *, lgt(s4,t1) ! { dg-error "must be of kind 1" }
  print *, lgt(s4,"foo") ! { dg-error "must be of kind 1" }
  print *, lgt(4_"foo",t1) ! { dg-error "must be of kind 1" }
  print *, lgt(4_"bar","foo") ! { dg-error "must be of kind 1" }

  print *, lgt(s4,t4) ! { dg-error "must be of kind 1" }
  print *, lgt(s4,4_"foo") ! { dg-error "must be of kind 1" }
  print *, lgt(4_"foo",t4) ! { dg-error "must be of kind 1" }
  print *, lgt(4_"bar",4_"foo") ! { dg-error "must be of kind 1" }

  print *, lle(s1,t1)
  print *, lle(s1,"foo")
  print *, lle("foo",t1)
  print *, lle("bar","foo")

  print *, lle(s1,t4) ! { dg-error "must be of kind 1" }
  print *, lle(s1,4_"foo") ! { dg-error "must be of kind 1" }
  print *, lle("foo",t4) ! { dg-error "must be of kind 1" }
  print *, lle("bar",4_"foo") ! { dg-error "must be of kind 1" }

  print *, lle(s4,t1) ! { dg-error "must be of kind 1" }
  print *, lle(s4,"foo") ! { dg-error "must be of kind 1" }
  print *, lle(4_"foo",t1) ! { dg-error "must be of kind 1" }
  print *, lle(4_"bar","foo") ! { dg-error "must be of kind 1" }

  print *, lle(s4,t4) ! { dg-error "must be of kind 1" }
  print *, lle(s4,4_"foo") ! { dg-error "must be of kind 1" }
  print *, lle(4_"foo",t4) ! { dg-error "must be of kind 1" }
  print *, lle(4_"bar",4_"foo") ! { dg-error "must be of kind 1" }

  print *, llt(s1,t1)
  print *, llt(s1,"foo")
  print *, llt("foo",t1)
  print *, llt("bar","foo")

  print *, llt(s1,t4) ! { dg-error "must be of kind 1" }
  print *, llt(s1,4_"foo") ! { dg-error "must be of kind 1" }
  print *, llt("foo",t4) ! { dg-error "must be of kind 1" }
  print *, llt("bar",4_"foo") ! { dg-error "must be of kind 1" }

  print *, llt(s4,t1) ! { dg-error "must be of kind 1" }
  print *, llt(s4,"foo") ! { dg-error "must be of kind 1" }
  print *, llt(4_"foo",t1) ! { dg-error "must be of kind 1" }
  print *, llt(4_"bar","foo") ! { dg-error "must be of kind 1" }

  print *, llt(s4,t4) ! { dg-error "must be of kind 1" }
  print *, llt(s4,4_"foo") ! { dg-error "must be of kind 1" }
  print *, llt(4_"foo",t4) ! { dg-error "must be of kind 1" }
  print *, llt(4_"bar",4_"foo") ! { dg-error "must be of kind 1" }

  print *, selected_char_kind("foo")
  print *, selected_char_kind(4_"foo") ! { dg-error "must be of kind 1" }
  print *, selected_char_kind(s1)
  print *, selected_char_kind(s4) ! { dg-error "must be of kind 1" }

  end
