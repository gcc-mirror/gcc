subroutine bla(a,bar,lb,ne,nt,v,b)
  character*8 lb
  integer bar(20),foo(8,5)
  real*8 a(3,*),x(3,8),v(0:3,*)
  if(lb(4:4).eq.'3') then
     n=8
  elseif(lb(4:5).eq.'10') then
     n=10
     ns=6
     m=4
  endif
  call blub(id)
  do
     if(id.eq.0) exit
     if(lb(4:4).eq.'6') then
        m=1
     endif
     if((n.eq.20).or.(n.eq.8)) then
        if(b.eq.0) then
           do i=1,ns
              do j=1,3
                 x(j,i)=a(j,bar(foo(i,ig)))
              enddo
           enddo
        else
           do i=1,ns
              do j=1,3
                 x(j,i)=a(j,bar(foo(i,ig)))+v(j,bar(foo(i,ig)))
              enddo
           enddo
        endif
     endif
     do i=1,m
        if(lb(4:5).eq.'1E') then
           call blab(x)
        endif
     enddo
  enddo
end subroutine bla
