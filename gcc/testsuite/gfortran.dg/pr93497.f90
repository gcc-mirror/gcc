! { dg-do compile } 

program p
   print *, [character(((/1/))) :: 'a','b'] ! { dg-error "Scalar INTEGER expression expected" }
   print *, [character(([1])) :: 'a','b']   ! { dg-error "Scalar INTEGER expression expected" }
   print *, [character(1+[1]) :: 'a','b']   ! { dg-error "Scalar INTEGER expression expected" }
end

