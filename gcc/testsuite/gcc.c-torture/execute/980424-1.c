Return-Path: amylaar@cygnus.co.uk Fri
Received: from cygnus.com (runyon.cygnus.com [205.180.230.5]) by hurl.cygnus.com with ESMTP (8.7.1/8.7.1) id JAA16352 for <law@hurl.cygnus.com>; Fri, 24 Apr 1998 09:55:26 -0600 (MDT)
Received: from pasanda.cygnus.co.uk (jaalfrezi.cygnus.co.uk [194.130.39.3])
	by runyon.cygnus.com (8.8.7-cygnus/8.8.7) with SMTP id IAA25956
	for <law@cygnus.com>; Fri, 24 Apr 1998 08:55:47 -0700 (PDT)
Received: (qmail 1227 invoked by alias); 24 Apr 1998 15:55:44 -0000
Received: (qmail 1215 invoked from network); 24 Apr 1998 15:55:43 -0000
Received: from phal.cygnus.co.uk (amylaar@194.130.39.5)
  by dns.cygnus.co.uk with SMTP; 24 Apr 1998 15:55:43 -0000
Received: (from amylaar@localhost)
	by phal.cygnus.co.uk (8.8.8/8.8.8) id QAA16241;
	Fri, 24 Apr 1998 16:55:18 +0100
From: Joern Rennecke <amylaar@cygnus.co.uk>
Message-Id: <199804241555.QAA16241@phal.cygnus.co.uk>
Subject: Another new c-torture test
To: tege@matematik.su.se (Torbjorn Granlund), law@cygnus.com (Jeffrey A Law)
Date: Fri, 24 Apr 1998 16:55:17 +0100 (BST)
X-Mailer: ELM [version 2.4ME+ PL37 (25)]
MIME-Version: 1.0
Content-Type: text/plain; charset=US-ASCII
Content-Transfer-Encoding: 7bit

This failed for SH at -O2:

int i, a[99];

void f (int one)
{
  if (one != 1)
    abort ();
}

void
g ()
{
  f (a[i & 0x3f]);
}

int
main ()
{
  a[0] = 1;
  i = 0x40;
  g ();
  exit (0);
}
