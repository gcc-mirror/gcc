! { dg-do run }
program b
   integer w
   character(len=2) s, t
   s = 'xi'

   w = scan(s, 'iI')
   if (w /= 2) call abort
   w = scan(s, 'xX', .true.)
   if (w /= 1) call abort
   w = scan(s, 'ab')
   if (w /= 0) call abort
   w = scan(s, 'ab', .true.)
   if (w /= 0) call abort

   s = 'xi'
   t = 'iI'
   w = scan(s, t)
   if (w /= 2) call abort
   t = 'xX'
   w = scan(s, t, .true.)
   if (w /= 1) call abort
   t = 'ab'
   w = scan(s, t)
   if (w /= 0) call abort
   w = scan(s, t, .true.)
   if (w /= 0) call abort

end program b
   

   
