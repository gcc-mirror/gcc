// Build don't link: 
// Special g++ Options: -pedantic-errors
// GROUPS passed casts
// casts file
// From: fjh@cs.mu.oz.au
// Message-Id: <9310121939.29641@munta.cs.mu.OZ.AU>
// Subject: should emit diagnostic for `int *p = (void *)0;'
// Date: Wed, 13 Oct 93 5:39:35 EST
        int *p = (void *)0;// ERROR - .*
