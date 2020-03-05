! PR ipa/84963
! { dg-additional-options "-O2" }

program p
   print *, sin([1.0, 2.0])
   print *, cos([1.0, 2.0])
end
