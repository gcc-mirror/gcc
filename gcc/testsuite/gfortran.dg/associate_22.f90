! { dg-do run }
program foo

   implicit none

   character(len=4) :: s
   character(len=10) :: a

   ! This works.
   s = 'abc'
   associate(t => s)
      if (trim(t) /= 'abc') STOP 1
   end associate

   ! This failed.
   associate(u => 'abc')
      if (trim(u) /= 'abc') STOP 2
   end associate

   ! This failed.
   a = s // 'abc'
   associate(v => s // 'abc')
      if (trim(v) /= trim(a)) STOP 3
   end associate

   ! This failed.
   a = trim(s) // 'abc'
   associate(w => trim(s) // 'abc')
      if (trim(w) /= trim(a)) STOP 4
   end associate

   ! This failed.
   associate(x => trim('abc'))
      if (trim(x) /= 'abc') STOP 5
   end associate

end program foo
