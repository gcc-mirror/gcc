* To: craig@jcb-sc.com
* Subject: Re: G77 and KIND=2
* Content-Type: text/plain; charset=us-ascii
* From: Dave Love <d.love@dl.ac.uk>
* Date: 03 Mar 1999 18:20:11 +0000
* In-Reply-To: craig@jcb-sc.com's message of "1 Mar 1999 21:04:38 -0000"
* User-Agent: Gnus/5.07007 (Pterodactyl Gnus v0.70) Emacs/20.3
* X-UIDL: d442bafe961c2a6ec6904f492e05d7b0
* 
* ISTM that there is a real problem printing integer*8 (on x86):
* 
* $ cat x.f
*[modified for test suite]
        integer *8 foo, bar
        data r/4e10/
        foo = 4e10
        bar = r
        if (foo .ne. bar) call abort
        end
* $ g77 x.f && ./a.out
*  1345294336
*  123
* $ f2c x.f && g77 x.c && ./a.out
* x.f:
*    MAIN:
*  40000000000
*  123
* $
* 
* Gdb shows the upper half of the buffer passed to do_lio is zeroed in
* the g77 case.
* 
* I've forgotten how the code generation happens.
