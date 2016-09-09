! { dg-do compile }
program foo
   print *, [character(len=*)::'ab','cd'] ! { dg-error "contain an asterisk" }
end program foo
