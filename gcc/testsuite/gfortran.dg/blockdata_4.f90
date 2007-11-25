! { dg-do compile }
! PR33152 Initialization/declaration problems in block data
! Test case prepared by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
blockdata bab
 character(len=3) :: myname(2)=(/'bar','baz'/)
 common/nmstr/myname
end blockdata bab

blockdata thdinit 
 implicit none 
 integer, parameter :: nmin=2 
 common/onestr/emname 
 character(len=3) :: emname(nmin) = (/'bar','baz'/) 
end blockdata thdinit

blockdata fooinit 
 implicit none 
 integer, parameter :: nmin=2 
 common/twostr/aname 
 data aname/'bar','baz'/ ! { dg-error "DATA array" }
 character(len=3) :: aname(nmin)
end blockdata fooinit

end
