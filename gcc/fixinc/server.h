
/*
 *  $Id: server.h,v 1.2 1998/12/16 21:19:17 law Exp $
 *
 *  Server Handling copyright 1992-1998 Bruce Korb
 *
 *  Server Handling is free software.
 *  You may redistribute it and/or modify it under the terms of the
 *  GNU General Public License, as published by the Free Software
 *  Foundation; either version 2, or (at your option) any later version.
 *
 *  Server Handling is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Server Handling.  See the file "COPYING".  If not,
 *  write to:  The Free Software Foundation, Inc.,
 *             59 Temple Place - Suite 330,
 *             Boston,  MA  02111-1307, USA.
 *
 * As a special exception, Bruce Korb gives permission for additional
 * uses of the text contained in his release of ServerHandler.
 *
 * The exception is that, if you link the ServerHandler library with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the ServerHandler library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by Bruce Korb under
 * the name ServerHandler.  If you copy code from other sources under the
 * General Public License into a copy of ServerHandler, as the General Public
 * License permits, the exception does not apply to the code that you add
 * in this way.  To avoid misleading anyone as to the status of such
 * modified files, you must delete this exception notice from them.
 *
 * If you write modifications of your own for ServerHandler, it is your
 * choice whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 */

#ifndef FIXINC_SERVER_H
#define FIXINC_SERVER_H

#include <stdio.h>
#include <unistd.h>

/*
 *  Dual pipe opening of a child process
 */

typedef struct
  {
    int readFd;
    int writeFd;
  }
tFdPair;

typedef struct
  {
    FILE *pfRead;		/* parent read fp  */
    FILE *pfWrite;		/* parent write fp */
  }
tpfPair;

typedef char *tpChar;

#define NOPROCESS   ((pid_t)-1)
#define NULLPROCESS ((pid_t)0)

#define EXIT_PANIC   99

typedef enum
  {
    BOOL_FALSE, BOOL_TRUE
  }
bool;

#define _P_(p) ()

char *runShell _P_ ((const char *pzCmd));
pid_t p2fopen _P_ ((tpfPair * pfPair, tpChar * ppArgs));
pid_t p2open _P_ ((tFdPair * pPair, tpChar * ppArgs));
int
chainOpen _P_ ((int stdinFd,
		tpChar * ppArgs,
		pid_t * pChild));

#endif /* FIXINC_SERVER_H */
