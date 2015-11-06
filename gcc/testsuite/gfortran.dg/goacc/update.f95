! { dg-do compile } 

program foo
  !$acc update ! { dg-error "must contain at least one 'device' or 'host' or 'self' clause" }
end program foo
