// Message-Id: <m0lLuUK-0000fRC@nix.appli.se>
// Date: Wed, 4 Mar 92 12:50 MET
// From: niklas@appli.se (Niklas Hallqvist)
// To: eichin@cygnus.com, tiemann@cygnus.com
// Cc: gcc2@cygnus.com
// Subject: nested type handling
// 
// The last couple of days I've been struggling with nested types in the
// C++ compiler.  Frankly, it's a mess!  Was it impossible to put the stuff
// into the parser instead of the lexer?  Well, anyway, to get the following
// code to compile:
// 
// struct O {
//   struct M {
//     struct I
//       {};
//   };
// };
// O::M::I s;
// 
// a patch enclosed below is needed.  I'm not sure if it causes any
// unanticipated side-effects, but it seem to work well for me.

// Build don't link:

struct O {
  struct M {
    struct I
      {};
  };
};
O::M::I s;
