/*
  pushback.h - header for pushback.c
  Copyright (C) 1999  Bryan Burns
  
  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

struct pb_file {
  unsigned int buff_amt;
  ub1 pb_buff[RDSZ];
  int fd;
  ub1 *next;
};

typedef struct pb_file pb_file;


void pb_init(pb_file *, int);
int pb_push(pb_file *, void *, int);
int pb_read(pb_file *, void *, int);
