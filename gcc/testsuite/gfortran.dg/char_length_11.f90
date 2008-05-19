! { dg-do compile }

  character(len=*), parameter :: s = "foo"
  write (*,*) adjustr(s(:))
end
