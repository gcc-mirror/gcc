// { dg-do assemble  }
// GROUPS passed operators
// pic file
// Message-Id: <199406132030.NAA23508@dewitt.eecs.berkeley.edu>
// Subject: gcc-2.5.8 -fpic fails to compile extern const char static initializer
// Date: Mon, 13 Jun 1994 13:30:14 -0700
// From: Christopher Hylands <cxh@dewitt.eecs.berkeley.edu>

extern const char SDFdomainName[] = "SDF";
