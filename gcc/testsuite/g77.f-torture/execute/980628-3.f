Return-Path: owner-egcs-patches@cygnus.com Sun
Received: from cygnus.com (runyon.cygnus.com [205.180.230.5]) by hurl.cygnus.com with ESMTP (8.7.1/8.7.1) id NAA10299 for <law@hurl.cygnus.com>; Sun, 28 Jun 1998 13:57:57 -0600 (MDT)
Received: (from majordom@localhost)
	by runyon.cygnus.com (8.8.7-cygnus/8.8.7) id KAA00196;
	Sun, 28 Jun 1998 10:30:03 -0700 (PDT)
Received: from mescaline.gnu.org (mescaline.gnu.org [158.121.106.21])
	by runyon.cygnus.com (8.8.7-cygnus/8.8.7) with ESMTP id KAA00177
	for <egcs-patches@cygnus.com>; Sun, 28 Jun 1998 10:30:00 -0700 (PDT)
Received: from melange.gnu.org by mescaline.gnu.org (8.8.5/8.6.12GNU) with ESMTP id NAA19196 for <egcs-patches@cygnus.com>; Sun, 28 Jun 1998 13:30:18 -0400
Received: (burley@localhost) by melange.gnu.org (8.8.5/8.6.12GNU) id NAA11244; Sun, 28 Jun 1998 13:29:59 -0400 (EDT)
Date: Sun, 28 Jun 1998 13:29:59 -0400 (EDT)
Message-Id: <199806281729.NAA11244@melange.gnu.org>
From: Craig Burley <burley@gnu.org>
To: egcs-patches@cygnus.com
Subject: New test (4th of 4) for g77 test suite
Sender: owner-egcs-patches@cygnus.com
Precedence: bulk

Jeff, could you add this as:

  egcs/gcc/testsuite/g77.f-torture/execute/980628-3.f

        tq vm, (burley)


* g77 0.5.23 and previous had bugs involving too little space
* allocated for EQUIVALENCE and COMMON areas needing initial
* padding to meet alignment requirements of the system.

      call subr
      end

      subroutine subr
      implicit none
      save

      character c1(11), c2(11), c3(11)
      real r1, r2, r3
      character c4, c5, c6
      equivalence (c1(2), r1)
      equivalence (c2(2), r2)
      equivalence (c3(2), r3)

      c1(1) = '1'
      r1 = 1.
      c1(11) = '1'
      c4 = '4'
      c2(1) = '2'
      r2 = 2.
      c2(11) = '2'
      c5 = '5'
      c3(1) = '3'
      r3 = 3.
      c3(11) = '3'
      c6 = '6'

      call x (c1, r1, c2, r2, c3, r3, c4, c5, c6)

      end

      subroutine x (c1, r1, c2, r2, c3, r3, c4, c5, c6)
      implicit none

      character c1(11), c2(11), c3(11)
      real r1, r2, r3
      character c4, c5, c6

      if (c1(1) .ne. '1') call abort
      if (r1 .ne. 1.) call abort
      if (c1(11) .ne. '1') call abort
      if (c4 .ne. '4') call abort
      if (c2(1) .ne. '2') call abort
      if (r2 .ne. 2.) call abort
      if (c2(11) .ne. '2') call abort
      if (c5 .ne. '5') call abort
      if (c3(1) .ne. '3') call abort
      if (r3 .ne. 3.) call abort
      if (c3(11) .ne. '3') call abort
      if (c6 .ne. '6') call abort

      end
