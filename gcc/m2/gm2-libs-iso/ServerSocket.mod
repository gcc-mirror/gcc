(* ServerSocket.mod implements the ability to open a socket as a Channel.

Copyright (C) 2008-2021 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE ServerSocket ;


(*
   OpenSocketBindListen - opens a TCP server socket.  The socket
                          is bound to, port, and will allow, listen,
                          pending connections.  The result of these
                          combined operations is returned in, res.
*)

PROCEDURE OpenSocketBindListen (VAR socketid: ChanId;
                                port: CARDINAL; listen: CARDINAL;
                                VAR res: OpenResults) ;
BEGIN
   
END OpenSocketBindListen ;


(*
   OpenAccept - attempts to open a new channel whose
                input/output capability is determined by,
                flags.  The result of this attempt is returned
                in res.
*)

PROCEDURE OpenAccept (VAR cid: ChanId; socketid: ChanId;
                      flags: FlagSet; VAR res: OpenResults) ;
BEGIN
   
END OpenAccept ;


(*
   Close - if the channel identified by cid was not opened as
           a server socket stream, the exception wrongDevice is
           raised; otherwise closes the channel, and assigns
           the value identifying the invalid channel to cid.
*)

PROCEDURE Close (VAR cid: ChanId) ;
BEGIN
   
END Close ;


(*
   IsSocket - tests if the channel identified by cid is open as
              a server socket stream.
*)

PROCEDURE IsSocket (cid: ChanId) : BOOLEAN ;
BEGIN
   RETURN TRUE
END IsSocket ;


END ServerSocket.
