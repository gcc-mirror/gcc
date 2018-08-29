c { dg-do run }
c { dg-options "-std=legacy" }
c
!  Some versions of cpp will delete "//'World' as a C++ comment.
      character*40    title
      title = 'Hello '//'World'
      if (title .ne. 'Hello World') STOP 1
      end
