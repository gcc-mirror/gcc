// Build don't link: 
// GROUPS passed initialization
// init file
// From: lupine!segfault!rfg@uunet.UU.NET
// Date:     Sun, 27 Sep 92 14:06:22 MDT
// Subject:  Bug in g++ (920220) handling reference initializers.
// Message-ID: <15409.717627982@segfault>

int * const & fiddle = 0;
