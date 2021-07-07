(* IO.mod provides Read, Write, Errors procedures mapping onto 0, 1 and 2.

Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE IO ;


FROM StrLib IMPORT StrCopy ;
FROM SYSTEM IMPORT ADR, SIZE ;
FROM libc IMPORT read, write, system, isatty ;

FROM FIO IMPORT File, StdIn, StdOut, StdErr, WriteChar, ReadChar,
                GetUnixFileDescriptor, FlushBuffer ;

FROM errno IMPORT geterrno, EINTR, EAGAIN ;
FROM ASCII IMPORT cr, eof, nl;
FROM termios IMPORT TERMIOS, Flag, InitTermios, KillTermios,
                    SetFlag, tcgetattr, tcsetattr, cfmakeraw,
                    tcsdrain, tcsnow, tcsflush ;


CONST
   MaxDefaultFd = 2 ;

TYPE
   BasicFds = RECORD
                 IsEof,
                 IsRaw: BOOLEAN ;
              END ;

VAR
   fdState: ARRAY [0..MaxDefaultFd] OF BasicFds ;


(*
   IsDefaultFd - returns TRUE if, fd, is 0, 1 or 2.
*)

PROCEDURE IsDefaultFd (fd: INTEGER) : BOOLEAN ;
BEGIN
   RETURN( (fd<=MaxDefaultFd) AND (fd>=0) )
END IsDefaultFd ;


PROCEDURE Read (VAR ch: CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   WITH fdState[0] DO
      FlushBuffer(StdOut) ;
      FlushBuffer(StdErr) ;
      IF IsRaw
      THEN
         IF IsEof
         THEN
            ch := eof
         ELSE
            LOOP
               r := read(GetUnixFileDescriptor(StdIn), ADR(ch), 1) ;
               IF r=1
               THEN
                  RETURN
               ELSIF r=-1
               THEN
                  r := geterrno() ;
                  IF r#EAGAIN
                  THEN
                     IsEof := TRUE ;
                     ch := eof ;
                     RETURN
                  END
               END
            END
         END
      ELSE
         ch := ReadChar(StdIn)
      END
   END
END Read ;


(*
   doWrite - performs the write of a single character, ch,
             onto fd or f.
*)

PROCEDURE doWrite (fd: INTEGER; f: File; ch: CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   WITH fdState[fd] DO
      IF IsRaw
      THEN
         IF NOT IsEof
         THEN
            LOOP
               r := write(GetUnixFileDescriptor(f), ADR(ch), 1) ;
               IF r=1
               THEN
                  RETURN
               ELSIF r=-1
               THEN
                  r := geterrno() ;
                  IF (r#EAGAIN) AND (r#EINTR)
                  THEN
                     IsEof := TRUE ;
                     RETURN
                  END
               END
            END
         END
      ELSE
         WriteChar(f, ch)
      END
   END
END doWrite ;


PROCEDURE Write (ch: CHAR) ;
BEGIN
   doWrite(1, StdOut, ch)
END Write ;


PROCEDURE Error (ch: CHAR) ;
BEGIN
   doWrite(2, StdErr, ch)
END Error ;


(*
   setFlag - sets or unsets the appropriate flag in, t.
*)

PROCEDURE setFlag (t: TERMIOS; f: Flag; b: BOOLEAN) ;
BEGIN
   IF SetFlag(t, f, b)
   THEN
   END
END setFlag ;


(*
   doraw - sets all the flags associated with making this
           file descriptor into raw input/output.
*)

PROCEDURE doraw (term: TERMIOS) ;
BEGIN
   (*
    * from man 3 termios
    *           termios_p->c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP
    *                                   | INLCR | IGNCR | ICRNL | IXON);
    *           termios_p->c_oflag &= ~OPOST;
    *           termios_p->c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
    *           termios_p->c_cflag &= ~(CSIZE | PARENB);
    *           termios_p->c_cflag |= CS8;
    *)
   setFlag(term, ignbrk, FALSE) ;
   setFlag(term, ibrkint, FALSE) ;
   setFlag(term, iparmrk, FALSE) ;
   setFlag(term, istrip, FALSE) ;
   setFlag(term, inlcr, FALSE) ;
   setFlag(term, igncr, FALSE) ;
   setFlag(term, icrnl, FALSE) ;
   setFlag(term, ixon, FALSE) ;

   setFlag(term, opost, FALSE) ;

   setFlag(term, lecho, FALSE) ;
   setFlag(term, lechonl, FALSE) ;
   setFlag(term, licanon, FALSE) ;
   setFlag(term, lisig, FALSE) ;
   setFlag(term, liexten, FALSE) ;

   setFlag(term, parenb, FALSE) ;
   setFlag(term, cs8, TRUE)
END doraw ;


(*
   dononraw - sets all the flags associated with making this
              file descriptor into non raw input/output.
*)

PROCEDURE dononraw (term: TERMIOS) ;
BEGIN
   (*
    * we undo these settings, (although we leave the character size alone)
    *
    * from man 3 termios
    *           termios_p->c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP
    *                                   | INLCR | IGNCR | ICRNL | IXON);
    *           termios_p->c_oflag &= ~OPOST;
    *           termios_p->c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
    *           termios_p->c_cflag &= ~(CSIZE | PARENB);
    *           termios_p->c_cflag |= CS8;
    *)
   setFlag(term, ignbrk, TRUE) ;
   setFlag(term, ibrkint, TRUE) ;
   setFlag(term, iparmrk, TRUE) ;
   setFlag(term, istrip, TRUE) ;
   setFlag(term, inlcr, TRUE) ;
   setFlag(term, igncr, TRUE) ;
   setFlag(term, icrnl, TRUE) ;
   setFlag(term, ixon, TRUE) ;

   setFlag(term, opost, TRUE) ;

   setFlag(term, lecho, TRUE) ;
   setFlag(term, lechonl, TRUE) ;
   setFlag(term, licanon, TRUE) ;
   setFlag(term, lisig, TRUE) ;
   setFlag(term, liexten, TRUE)
END dononraw ;


PROCEDURE BufferedMode (fd: INTEGER; input: BOOLEAN) ;
VAR
   term: TERMIOS ;
   r   : INTEGER ;
BEGIN
   IF IsDefaultFd(fd)
   THEN
      fdState[fd].IsRaw := FALSE
   END ;
   term := InitTermios() ;
   IF tcgetattr(fd, term)=0
   THEN
      dononraw(term) ;
      IF input
      THEN
         r := tcsetattr(fd, tcsflush(), term)
      ELSE
         r := tcsetattr(fd, tcsdrain(), term)
      END
   END ;
   term := KillTermios(term)
END BufferedMode ;


PROCEDURE UnBufferedMode (fd: INTEGER; input: BOOLEAN) ;
VAR
   term  : TERMIOS ;
   result: INTEGER ;
BEGIN
   IF IsDefaultFd(fd)
   THEN
      fdState[fd].IsRaw := TRUE
   END ;
   term := InitTermios() ;
   IF tcgetattr(fd, term)=0
   THEN
      doraw(term) ;
      IF input
      THEN
         result := tcsetattr(fd, tcsflush(), term)
      ELSE
         result := tcsetattr(fd, tcsdrain(), term)
      END
   END ;
   term := KillTermios(term)
END UnBufferedMode ;


(*
   EchoOn - turns on echoing for file descriptor, fd.  This
            only really makes sence for a file descriptor opened
            for terminal input or maybe some specific file descriptor
            which is attached to a particular piece of hardware.
*)

PROCEDURE EchoOn (fd: INTEGER; input: BOOLEAN) ;
VAR
   term  : TERMIOS ;
   result: INTEGER ;
BEGIN
   term := InitTermios() ;
   IF tcgetattr(fd, term)=0
   THEN
      setFlag(term, lecho, TRUE) ;
      IF input
      THEN
         result := tcsetattr(fd, tcsflush(), term)
      ELSE
         result := tcsetattr(fd, tcsdrain(), term)
      END
   END ;
   term := KillTermios(term)
END EchoOn ;


(*
   EchoOff - turns off echoing for file descriptor, fd.  This
             only really makes sence for a file descriptor opened
             for terminal input or maybe some specific file descriptor
             which is attached to a particular piece of hardware.
*)

PROCEDURE EchoOff (fd: INTEGER; input: BOOLEAN) ;
VAR
   term  : TERMIOS ;
   result: INTEGER ;
BEGIN
   term := InitTermios() ;
   IF tcgetattr(fd, term)=0
   THEN
      setFlag(term, lecho, FALSE) ;
      IF input
      THEN
         result := tcsetattr(fd, tcsflush(), term)
      ELSE
         result := tcsetattr(fd, tcsdrain(), term)
      END
   END ;
   term := KillTermios(term)
END EchoOff ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   WITH fdState[0] DO
      IsEof := FALSE ;
      IsRaw := FALSE
   END ;
   WITH fdState[1] DO
      IsEof := FALSE ;
      IsRaw := FALSE
   END ;
   WITH fdState[2] DO
      IsEof := FALSE ;
      IsRaw := FALSE
   END
END Init ;


BEGIN
   Init
END IO.
