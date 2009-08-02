! { dg-do compile }
! { dg-options "-std=legacy" }
!
! Testing g77 intrinsics as subroutines
      integer(kind=8) i8, j8
      integer i4, j4
      integer i, j
      character*80 c

      call gerror (c)
      call getlog (c)

      call hostnm (c, status = i8)
      call hostnm (c, i8)
      call hostnm (c, status = i4)
      call hostnm (c, i4)
      call hostnm (c, status = i)
      call hostnm (c, i)
      call hostnm (c)

      call kill (i8, i8, status = i8)
      call kill (i8, i8, i8)
      call kill (i4, i8, i8)
      call kill (i8, i4, i8)
      call kill (i8, i8, i4)
      call kill (i4, i4, i8)
      call kill (i4, i8, i4)
      call kill (i8, i4, i4)
      call kill (i4, i4, i4)
      call kill (i, i, i)
      call kill (i8, i8)
      call kill (i4, i8)
      call kill (i8, i4)
      call kill (i4, i4)
      call kill (i, i)

      call link ('foo', 'bar', status = i8)
      call link ('foo', 'bar', status = i4)
      call link ('foo', 'bar', status = i)
      call link ('foo', 'bar', i8)
      call link ('foo', 'bar', i4)
      call link ('foo', 'bar', i)
      call link ('foo', 'bar')

      call perror (c)
      
      call rename ('foo', 'bar', status = i8)
      call rename ('foo', 'bar', status = i4)
      call rename ('foo', 'bar', status = i)
      call rename ('foo', 'bar', i8)
      call rename ('foo', 'bar', i4)
      call rename ('foo', 'bar', i)
      call rename ('foo', 'bar')

      i = 1
      i4 = 1
      i8 = 1
      call sleep (i)
      call sleep (i4)
      call sleep (i8)
      call sleep (-1)

      call symlnk ('foo', 'bar', status = i8)
      call symlnk ('foo', 'bar', status = i4)
      call symlnk ('foo', 'bar', status = i)
      call symlnk ('foo', 'bar', i8)
      call symlnk ('foo', 'bar', i4)
      call symlnk ('foo', 'bar', i)
      call symlnk ('foo', 'bar')

! Cleaning our mess
      call unlink ('bar')

! This should be the last test, unless you want garbage everywhere in
! your filesystem.
      call chdir ('..', status = i8)
      call chdir ('..', i8)
      call chdir ('..', status = i4)
      call chdir ('..', i4)
      call chdir ('..', status = i)
      call chdir ('..', i)
      call chdir ('..')

      end
