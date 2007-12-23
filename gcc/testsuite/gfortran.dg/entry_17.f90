function test1(n)
  integer  :: n
  character(n) :: test1
  character(n) :: bar1
  test1 = ""
  return
entry bar1()
  bar1 = ""
end function test1

function test2()
  character(1) :: test2
  character(1) :: bar2
  test2 = ""
  return
entry bar2()
  bar2 = ""
end function test2

function test3() ! { dg-warning "is obsolescent" }
  character(*) :: test3
  character(*) :: bar3 ! { dg-warning "is obsolescent" }
  test3 = ""
  return
entry bar3()
  bar3 = ""
end function test3 ! { dg-warning "is obsolescent" }

function test4(n) ! { dg-error "returning variables of different string lengths" }
  integer  :: n
  character(n) :: test4
  character(*) :: bar4 ! { dg-warning "is obsolescent" }
  test4 = ""
  return
entry bar4()
  bar4 = ""
end function test4

function test5() ! { dg-error "returning variables of different string lengths" }
  character(1) :: test5
  character(2) :: bar5
  test5 = ""
  return
entry bar5()
  bar5 = ""
end function test5

function test6() ! { dg-warning "is obsolescent|returning variables of different string lengths" }
  character(*) :: test6
  character(2) :: bar6
  test6 = ""
  return
entry bar6()
  bar6 = ""
end function test6 ! { dg-warning "is obsolescent" }
