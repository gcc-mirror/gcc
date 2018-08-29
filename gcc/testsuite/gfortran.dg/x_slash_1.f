c { dg-do run { target fd_truncate } }
c { dg-options "-std=legacy" }
c
c This program tests the fixes to PR22570.
c
c Provided by Paul Thomas - pault@gcc.gnu.org
c
       program x_slash
       character*60 a
       character*1  b, c

       open (10, status = "scratch")

c Check that lines with only x-editing followed by a slash generate
c spaces and that subsequent lines have spaces where they should.
c Line 1 we ignore.
c Line 2 has nothing but x editing, followed by a slash.
c Line 3 has x editing finished off by a 1h*

       write (10, 100)
 100   format (1h1,58x,1h!,/,60x,/,59x,1h*,/)
       rewind (10)

       read (10, 200) a
       read (10, 200) a
       do i = 1,60
         if (ichar(a(i:i)).ne.32) STOP 1
       end do
       read (10, 200) a
 200   format (a60)
       do i = 1,59
         if (ichar(a(i:i)).ne.32) STOP 2
       end do
       if (a(60:60).ne."*") STOP 3
       rewind (10)

c Check that sequences of t- and x-editing generate the correct 
c number of spaces.
c Line 1 we ignore.
c Line 2 has tabs to the right of present position.
c Line 3 has tabs to the left of present position.

       write (10, 101)
 101   format (1h1,58x,1h#,/,t38,2x,1h ,tr10,9x,1h$,/,
     >         6habcdef,tl4,2x,6hghijkl,t1,59x,1h*)
       rewind (10)

       read (10, 200) a
       read (10, 200) a
       do i = 1,59
         if (ichar(a(i:i)).ne.32) STOP 4
       end do
       if (a(60:60).ne."$") STOP 5
       read (10, 200) a
       if (a(1:10).ne."abcdghijkl") STOP 6
       do i = 11,59
         if (ichar(a(i:i)).ne.32) STOP 7
       end do
       if (a(60:60).ne."*") STOP 8
       rewind (10)

c Now repeat the first test, with the write broken up into three
c separate statements. This checks that the position counters are
c correctly reset for each statement.

       write (10,102) "#"
       write (10,103)
       write (10,102) "$"
 102   format(59x,a1)
 103   format(60x)
       rewind (10)
       read (10, 200) a
       read (10, 200) a
       read (10, 200) a
       do i = 11,59
         if (ichar(a(i:i)).ne.32) STOP 9
       end do
       if (a(60:60).ne."$") STOP 10
       rewind (10)

c Next we check multiple read x- and t-editing.
c First, tab to the right.

       read (10, 201) b, c
201    format (tr10,49x,a1,/,/,2x,t60,a1)
       if ((b.ne."#").or.(c.ne."$")) STOP 11
       rewind (10)

c Now break it up into three reads and use left tabs.

       read (10, 202) b
202    format (10x,tl10,59x,a1)
       read (10, 203)
203    format ()
       read (10, 204) c
204    format (10x,t5,55x,a1)
       if ((b.ne."#").or.(c.ne."$")) STOP 12
       close (10)

c Now, check that trailing spaces are not transmitted when we have
c run out of data (Thanks to Jack Howarth for finding this one:
c http://gcc.gnu.org/ml/fortran/2005-07/msg00395.html).

       open (10, pad = "no", status = "scratch")
       b = achar (0)
       write (10, 105) 42
  105  format (i10,1x,i10)
       write (10, 106)
  106  format ("============================")
       rewind (10)
       read (10, 205, iostat = ier) i, b
  205  format (i10,a1)
       if ((ier.eq.0).or.(ichar(b).ne.0)) STOP 13

c That's all for now, folks! 

       end

