!  Some versions of cpp will delete "//'World' as a C++ comment.
      character*40    title
      title = 'Hello '//'World'
      if (title .ne. 'Hello World') stop 1
      end
