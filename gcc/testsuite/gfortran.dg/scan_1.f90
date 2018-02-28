! { dg-do run }
program b
   integer w
   character(len=2) s, t
   s = 'xi'

   w = scan(s, 'iI')
   if (w /= 2) STOP 1
   w = scan(s, 'xX', .true.)
   if (w /= 1) STOP 2
   w = scan(s, 'ab')
   if (w /= 0) STOP 3
   w = scan(s, 'ab', .true.)
   if (w /= 0) STOP 4

   s = 'xi'
   t = 'iI'
   w = scan(s, t)
   if (w /= 2) STOP 5
   t = 'xX'
   w = scan(s, t, .true.)
   if (w /= 1) STOP 6
   t = 'ab'
   w = scan(s, t)
   if (w /= 0) STOP 7
   w = scan(s, t, .true.)
   if (w /= 0) STOP 8

end program b
   

   
