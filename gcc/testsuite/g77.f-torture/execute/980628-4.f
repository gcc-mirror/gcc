Return-Path: owner-egcs-patches@cygnus.com Sun
Received: from cygnus.com (runyon.cygnus.com [205.180.230.5]) by hurl.cygnus.com with ESMTP (8.7.1/8.7.1) id MAA10053 for <law@hurl.cygnus.com>; Sun, 28 Jun 1998 12:17:16 -0600 (MDT)
Received: (from majordom@localhost)
	by runyon.cygnus.com (8.8.7-cygnus/8.8.7) id KAA00502;
	Sun, 28 Jun 1998 10:47:22 -0700 (PDT)
Received: from mescaline.gnu.org (mescaline.gnu.org [158.121.106.21])
	by runyon.cygnus.com (8.8.7-cygnus/8.8.7) with ESMTP id KAA00498
	for <egcs-patches@cygnus.com>; Sun, 28 Jun 1998 10:47:20 -0700 (PDT)
Received: from melange.gnu.org by mescaline.gnu.org (8.8.5/8.6.12GNU) with ESMTP id NAA19412 for <egcs-patches@cygnus.com>; Sun, 28 Jun 1998 13:47:38 -0400
Received: (burley@localhost) by melange.gnu.org (8.8.5/8.6.12GNU) id NAA11299; Sun, 28 Jun 1998 13:47:18 -0400 (EDT)
Date: Sun, 28 Jun 1998 13:47:18 -0400 (EDT)
Message-Id: <199806281747.NAA11299@melange.gnu.org>
From: Craig Burley <burley@gnu.org>
To: egcs-patches@cygnus.com
Subject: More g77 tests (1 of 2)
Sender: owner-egcs-patches@cygnus.com
Precedence: bulk

Jeff, could you please install this as:

  egcs/gcc/testsuite/g77-f.torture/execute/980628-4.f

        tq vm, (burley)


* g77 0.5.23 and previous had bugs involving too little space
* allocated for EQUIVALENCE and COMMON areas needing initial
* padding to meet alignment requirements of the system,
* including when initial values are provided (e.g. DATA).

      program test
      implicit none

      real r
      double precision d
      common /cmn/ r, d

      if (r .ne. 1.) call abort
      if (d .ne. 10.) call abort

      end

      block data init
      implicit none

      real r
      double precision d
      common /cmn/ r, d

      data r/1./, d/10./

      end
