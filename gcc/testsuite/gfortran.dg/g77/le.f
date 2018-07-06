c { dg-do run }
      program fool

      real     foo
      integer  n
      logical  t

      foo = 2.5
      n = 5

      t = (n > foo)
      if (t .neqv. .true.) STOP 1
      t = (n >= foo)
      if (t .neqv. .true.) STOP 2
      t = (n < foo)
      if (t .neqv. .false.) STOP 3
      t = (n <= 5)
      if (t .neqv. .true.) STOP 4
      t = (n >= 5 )
      if (t .neqv. .true.) STOP 5
      t = (n == 5)
      if (t .neqv. .true.) STOP 6
      t = (n /= 5)
      if (t .neqv. .false.) STOP 7
      t = (n /= foo)
      if (t .neqv. .true.) STOP 8
      t = (n == foo)
      if (t .neqv. .false.) STOP 9

      end
