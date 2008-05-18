! { dg-do compile }
! { dg-options "-fmax-errors=1000" }

program failme

  integer :: i, array(20)
  integer(kind=4) :: i4
  integer(kind=8) :: i8
  character(kind=1,len=20) :: s1, t1
  character(kind=4,len=20) :: s4, t4

  print *, access (s1, t1)
  print *, access (s1, t4) ! { dg-error "must be of kind" }
  print *, access (s4, t1) ! { dg-error "must be of kind" }
  print *, access (s4, t4) ! { dg-error "must be of kind" }

  print *, chdir (s1)
  print *, chdir (s4) ! { dg-error "must be of kind" }

  print *, chmod (s1, t1)
  print *, chmod (s1, t4) ! { dg-error "must be of kind" }
  print *, chmod (s4, t1) ! { dg-error "must be of kind" }
  print *, chmod (s4, t4) ! { dg-error "must be of kind" }

  print *, fget (s1)
  print *, fget (s4) ! { dg-error "must be of kind" }

  print *, fgetc (i, s1)
  print *, fgetc (i, s4) ! { dg-error "must be of kind" }

  print *, fput (s1)
  print *, fput (s4) ! { dg-error "must be of kind" }

  print *, fputc (i, s1)
  print *, fputc (i, s4) ! { dg-error "must be of kind" }

  print *, getcwd (s1)
  print *, getcwd (s4) ! { dg-error "Type of argument" }

  print *, hostnm (s1)
  print *, hostnm (s4) ! { dg-error "must be of kind" }

  print *, link (s1, t1)
  print *, link (s1, t4) ! { dg-error "must be of kind" }
  print *, link (s4, t1) ! { dg-error "must be of kind" }
  print *, link (s4, t4) ! { dg-error "must be of kind" }

  print *, lstat (s1, array)
  print *, lstat (s4, array) ! { dg-error "must be of kind" }
  print *, stat (s1, array)
  print *, stat (s4, array) ! { dg-error "must be of kind" }

  print *, rename (s1, t1)
  print *, rename (s1, t4) ! { dg-error "must be of kind" }
  print *, rename (s4, t1) ! { dg-error "must be of kind" }
  print *, rename (s4, t4) ! { dg-error "must be of kind" }

  print *, symlnk (s1, t1)
  print *, symlnk (s1, t4) ! { dg-error "must be of kind" }
  print *, symlnk (s4, t1) ! { dg-error "must be of kind" }
  print *, symlnk (s4, t4) ! { dg-error "must be of kind" }

  print *, system (s1)
  print *, system (s4) ! { dg-error "Type of argument" }

  print *, unlink (s1)
  print *, unlink (s4) ! { dg-error "must be of kind" }

end program failme
