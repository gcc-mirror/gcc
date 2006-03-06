! { dg-do compile }
       subroutine foo (a)
       real t, a, baz
       call bar (*10)
       t = 2 * baz ()
       IF (t.gt.0) t = baz ()
10     END
