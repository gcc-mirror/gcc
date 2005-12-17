! { dg-do compile }
! Testing g77 intrinsics as subroutines
      integer(kind=8) i8
      integer i4
      integer i
      character*80 c

      i8 = time ()
      i4 = time ()
      i8 = time8 ()
      i4 = time8 ()

      i8 = hostnm (c)
      i4 = hostnm (c)
      i = hostnm (c)

      i8 = ierrno ()
      i4 = ierrno ()
      i = ierrno ()

      i8 = kill (i8, i8)
      i8 = kill (i8, i4)
      i8 = kill (i4, i8)
      i8 = kill (i4, i4)
      i4 = kill (i8, i8)
      i4 = kill (i8, i4)
      i4 = kill (i4, i8)
      i4 = kill (i4, i4)

      i8 = link ('foo', 'bar')
      i4 = link ('foo', 'bar')
      i = link ('foo', 'bar')

      i8 = rename ('foo', 'bar')
      i4 = rename ('foo', 'bar')
      i = rename ('foo', 'bar')
      
      i8 = symlnk ('foo', 'bar')
      i4 = symlnk ('foo', 'bar')
      i = symlnk ('foo', 'bar')
      
! Cleaning our mess
      call unlink ('bar')

! This should be the last test, unless you want garbage everywhere in
! your filesystem.
      i8 = chdir ('..')
      i4 = chdir ('..')
      i = chdir ('..')

      end
