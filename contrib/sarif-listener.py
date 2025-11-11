#!/usr/bin/env python3

# Example listener for SARIF output to a socket.
# Copyright (C) 2025 Free Software Foundation, Inc.
# Contributed by David Malcolm <dmalcolm@redhat.com>.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# GCC is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.  */

from pathlib import Path
from socketserver import UnixStreamServer, StreamRequestHandler, ThreadingMixIn
import tempfile

class MyHandler(StreamRequestHandler):
    def handle(self):
        notification = self.rfile.read()
        print(notification)

class ThreadedUnixStreamServer(ThreadingMixIn, UnixStreamServer):
    pass

if __name__ == '__main__':
    with tempfile.TemporaryDirectory() as tmpdir:
        socket_path = Path(tmpdir) / 'socket'
        with ThreadedUnixStreamServer(socket_path.as_posix(),
                                      MyHandler) as server:
            print('listening on socket: %s' % socket_path)
            server.serve_forever()
