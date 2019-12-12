! { dg-do run }
!
! Test the fix for PR58618 by checking that substring associate targets
! work correctly.
!
! Contributed by Vladimir Fuka  <vladimir.fuka@gmail.com>
!
    character(5) :: s(2) = ['abcde','fghij']
    character (6), pointer :: ptr => NULL()
    character (6), target :: tgt = 'lmnopq'

    associate (x=>s(2)(3:4))
      if (x .ne. 'hi') stop 1
      x = 'uv'
    end associate
    if (any (s .ne. ['abcde','fguvj'])) stop 2

! Unity based substrings are cast differently.  */
    associate (x=>s(1)(1:4))
      if (x .ne. 'abcd') stop 3
      x(2:3) = 'wx'
    end associate
    if (any (s .ne. ['awxde','fguvj'])) stop 4

! Make sure that possible misidentifications do not occur.
    ptr => tgt
    associate (x=>ptr)
      if (x .ne. 'lmnopq') stop 5
      x(2:3) = 'wx'
    end associate
    if (tgt .ne. 'lwxopq') stop 6

    associate (x=>ptr(5:6))
      if (x .ne. 'pq') stop 7
      x = 'wx'
    end associate
    if (tgt .ne. 'lwxowx') stop 8
  end
