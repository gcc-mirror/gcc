! { dg-do compile }
! { dg-options "-fmax-errors=1000" }

program failme

  integer :: i, j, array(20)
  integer(kind=4) :: i4
  integer(kind=8) :: i8
  character(kind=1,len=20) :: s1, t1
  character(kind=4,len=20) :: s4, t4

  call ctime (i8, s1)
  call ctime (i8, s4) ! { dg-error "must be of kind" }

  call chdir (s1)
  call chdir (s1, i)
  call chdir (s4) ! { dg-error "must be of kind" }
  call chdir (s4, i) ! { dg-error "must be of kind" }

  call chmod (s1, t1)
  call chmod (s1, t4) ! { dg-error "must be of kind" }
  call chmod (s4, t1) ! { dg-error "must be of kind" }
  call chmod (s4, t4) ! { dg-error "must be of kind" }
  call chmod (s1, t1, i)
  call chmod (s1, t4, i) ! { dg-error "must be of kind" }
  call chmod (s4, t1, i) ! { dg-error "must be of kind" }
  call chmod (s4, t4, i) ! { dg-error "must be of kind" }

  call fdate (s1)
  call fdate (s4) ! { dg-error "must be of kind" }

  call gerror (s1)
  call gerror (s4) ! { dg-error "must be of kind" }

  call getcwd (s1)
  call getcwd (s1, i)
  call getcwd (s4) ! { dg-error "must be of kind" }
  call getcwd (s4, i) ! { dg-error "must be of kind" }

  call getenv (s1, t1)
  call getenv (s1, t4) ! { dg-error "Type of argument" }
  call getenv (s4, t1) ! { dg-error "Type of argument" }
  call getenv (s4, t4) ! { dg-error "Type of argument" }

  call getarg (i, s1)
  call getarg (i, s4) ! { dg-error "must be of kind" }

  call getlog (s1)
  call getlog (s4) ! { dg-error "must be of kind" }

  call fgetc (j, s1)
  call fgetc (j, s1, i)
  call fgetc (j, s4) ! { dg-error "must be of kind" }
  call fgetc (j, s4, i) ! { dg-error "must be of kind" }

  call fget (s1)
  call fget (s1, i)
  call fget (s4) ! { dg-error "must be of kind" }
  call fget (s4, i) ! { dg-error "must be of kind" }

  call fputc (j, s1)
  call fputc (j, s1, i)
  call fputc (j, s4) ! { dg-error "must be of kind" }
  call fputc (j, s4, i) ! { dg-error "must be of kind" }

  call fput (s1)
  call fput (s1, i)
  call fput (s4) ! { dg-error "must be of kind" }
  call fput (s4, i) ! { dg-error "must be of kind" }

  call hostnm (s1)
  call hostnm (s1, i)
  call hostnm (s4) ! { dg-error "must be of kind" }
  call hostnm (s4, i) ! { dg-error "must be of kind" }

  call link (s1, t1)
  call link (s1, t4) ! { dg-error "must be of kind" }
  call link (s4, t1) ! { dg-error "must be of kind" }
  call link (s4, t4) ! { dg-error "must be of kind" }
  call link (s1, t1, i)
  call link (s1, t4, i) ! { dg-error "must be of kind" }
  call link (s4, t1, i) ! { dg-error "must be of kind" }
  call link (s4, t4, i) ! { dg-error "must be of kind" }

  call perror (s1)
  call perror (s4) ! { dg-error "must be of kind" }

  call rename (s1, t1)
  call rename (s1, t4) ! { dg-error "must be of kind" }
  call rename (s4, t1) ! { dg-error "must be of kind" }
  call rename (s4, t4) ! { dg-error "must be of kind" }
  call rename (s1, t1, i)
  call rename (s1, t4, i) ! { dg-error "must be of kind" }
  call rename (s4, t1, i) ! { dg-error "must be of kind" }
  call rename (s4, t4, i) ! { dg-error "must be of kind" }

  call lstat (s1, array)
  call lstat (s1, array, i)
  call lstat (s4, array) ! { dg-error "must be of kind" }
  call lstat (s4, array, i) ! { dg-error "must be of kind" }

  call stat (s1, array)
  call stat (s1, array, i)
  call stat (s4, array) ! { dg-error "must be of kind" }
  call stat (s4, array, i) ! { dg-error "must be of kind" }

  call symlnk (s1, t1)
  call symlnk (s1, t4) ! { dg-error "must be of kind" }
  call symlnk (s4, t1) ! { dg-error "must be of kind" }
  call symlnk (s4, t4) ! { dg-error "must be of kind" }
  call symlnk (s1, t1, i)
  call symlnk (s1, t4, i) ! { dg-error "must be of kind" }
  call symlnk (s4, t1, i) ! { dg-error "must be of kind" }
  call symlnk (s4, t4, i) ! { dg-error "must be of kind" }

  call system (s1)
  call system (s1, i)
  call system (s4) ! { dg-error "Type of argument" }
  call system (s4, i) ! { dg-error "Type of argument" }

  call ttynam (i, s1)
  call ttynam (i, s4) ! { dg-error "must be of kind" }

  call unlink (s1)
  call unlink (s1, i)
  call unlink (s4) ! { dg-error "must be of kind" }
  call unlink (s4, i) ! { dg-error "must be of kind" }

end program failme
