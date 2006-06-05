! { dg-do compile }
  integer,parameter :: n = 5, m = 8
  integer a(10), i

  print *, a(15:14) ! don't warn
  print *, a(14:15) ! { dg-warning "is out of bounds" }
  print *, a(-5:-6) ! don't warn
  print *, a(-6:-5) ! { dg-warning "is out of bounds" }
  print *, a(15:14:1) ! don't warn
  print *, a(14:15:1) ! { dg-warning "is out of bounds" }
  print *, a(-5:-6:1) ! don't warn
  print *, a(-6:-5:1) ! { dg-warning "is out of bounds" }
  print *, a(15:14:-1) ! { dg-warning "is out of bounds" }
  print *, a(14:15:-1) ! don't warn
  print *, a(-5:-6:-1) ! { dg-warning "is out of bounds" }
  print *, a(-6:-5:-1) ! don't warn

  print *, a(15:) ! don't warn
  print *, a(15::-1) ! { dg-warning "is out of bounds" }
  print *, a(-1:) ! { dg-warning "is out of bounds" }
  print *, a(-1::-1) ! don't warn
  print *, a(:-1) ! don't warn
  print *, a(:-1:-1) ! { dg-warning "is out of bounds" }
  print *, a(:11) ! { dg-warning "is out of bounds" }
  print *, a(:11:-1) ! don't warn

  print *, a(1:20:10) ! { dg-warning "is out of bounds" }
  print *, a(1:15:15) ! don't warn
  print *, a(1:16:15) ! { dg-warning "is out of bounds" }
  print *, a(10:15:6) ! don't warn
  print *, a(11:15:6) ! { dg-warning "is out of bounds" }
  print *, a(11:-5:6) ! don't warn

  print *, a(10:-8:-9) ! { dg-warning "is out of bounds" }
  print *, a(10:-7:-9) ! don't warn
  
  print *, a(0:0:-1) ! { dg-warning "is out of bounds" }
  print *, a(0:0:1) ! { dg-warning "is out of bounds" }
  print *, a(0:0) ! { dg-warning "is out of bounds" }

  print *, a(1:15:i) ! don't warn
  print *, a(1:15:n) ! { dg-warning "is out of bounds" }
  print *, a(1:15:m) ! don't warn

  print *, a(1:-5:-m) ! don't warn
  print *, a(1:-5:-n) ! { dg-warning "is out of bounds" }
  print *, a(1:-5:-i) ! don't warn

  print *, a(-5:-5) ! { dg-warning "is out of bounds" }
  print *, a(15:15) ! { dg-warning "is out of bounds" }
  print *, a(-5:-5:1) ! { dg-warning "is out of bounds" }
  print *, a(15:15:-1) ! { dg-warning "is out of bounds" }
  print *, a(-5:-5:2) ! { dg-warning "is out of bounds" }
  print *, a(15:15:-2) ! { dg-warning "is out of bounds" }
  print *, a(-5:-5:n) ! { dg-warning "is out of bounds" }
  print *, a(15:15:-n) ! { dg-warning "is out of bounds" }
  print *, a(-5:-5:i) ! { dg-warning "is out of bounds" }
  print *, a(15:15:-i) ! { dg-warning "is out of bounds" }
  print *, a(5:5) ! don't warn
  print *, a(5:5:1) ! don't warn
  print *, a(5:5:-1) ! don't warn
  print *, a(5:5:2) ! don't warn
  print *, a(5:5:-2) ! don't warn
  print *, a(5:5:n) ! don't warn
  print *, a(5:5:-n) ! don't warn
  print *, a(5:5:i) ! don't warn
  print *, a(5:5:-i) ! don't warn

  end
