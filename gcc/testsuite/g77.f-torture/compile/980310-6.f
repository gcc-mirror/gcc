C From: Norbert Conrad <Norbert.Conrad@hrz.uni-giessen.de>
C Message-Id: <199711131008.LAA12272@marvin.hrz.uni-giessen.de>
C Subject: 971105  g77 bug
C To: egcs-bugs@cygnus.com
C Date: Thu, 13 Nov 1997 11:08:19 +0100 (CET)

C I found a bug in g77 in snapshot 971105

      subroutine ai (a)
      dimension a(-1:*)
      return
      end
C ai.f: In subroutine `ai':
C ai.f:1: 
C          subroutine ai (a)
C                         ^
C Array `a' at (^) is too large to handle
C 
C This happens whenever the lower index boundary is negative and the upper index
C boundary is '*'. 

