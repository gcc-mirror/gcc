// { dg-do assemble }
// GROUPS passed arrays
// array file
// Message-Id: <"nac.no.188:05.10.92.14.37.45"@nac.no>
// From: frode@auticon.no
// Subject: prototype bug ?
// Date: Thu, 5 Nov 92 15:37:34 PST

extern unsigned char * (*trt_def(int))[][2];
extern unsigned char * (trt_rplst(unsigned char *(*)[][2])); // { dg-error "" "" { target c++14_down } }
