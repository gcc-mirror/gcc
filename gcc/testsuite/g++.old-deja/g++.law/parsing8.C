// Build don't link: 
// GROUPS passed parsing
// parsing folder
// From: nag@soft.flab.fujitsu.co.jp
// Date:     Thu, 08 Jul 1993 10:54:59 +0900
// Subject:  g++ cannot understand `void (**f)()'
// Message-ID: <9307080155.AA00496@kumade.soft.flab.fujitsu.co.jp>

        void
        func() {
                int ( * * i )[ 2 ];
        }

// Looks like this is probably the same problem
// parsing folder
// From: nag@soft.flab.fujitsu.co.jp
// Date:     Thu, 08 Jul 1993 10:54:59 +0900
// Subject:  g++ cannot understand `void (**f)()'
// Message-ID: <9307080155.AA00496@kumade.soft.flab.fujitsu.co.jp>
int main()
{
        void (**f)();
}


// Same as
// From: Chris Dodd <dodd@csl.sri.com>
// Date:     Fri, 16 Jul 93 17:05:04 -0700
// Subject:  bug in declaration parsing in g++ 2.4.5
// Message-ID: <9307170005.AA03857@pekoe.csl.sri.com>

