/* Examples adapted from https://cwe.mitre.org/data/definitions/131.html
   which states "Copyright © 2006–2022, The MITRE Corporation. CWE, CWSS, CWRAF, and the CWE logo are trademarks of The MITRE Corporation."
   and which has this on:
     https://cwe.mitre.org/about/termsofuse.html

   Terms of Use

   CWE™ is free to use by any organization or individual for any research, development, and/or commercial purposes, per these CWE Terms of Use. The MITRE Corporation ("MITRE") has copyrighted the CWE List, Top 25, CWSS, and CWRAF for the benefit of the community in order to ensure each remains a free and open standard, as well as to legally protect the ongoing use of it and any resulting content by government, vendors, and/or users. CWE is a trademark of MITRE. Please contact cwe@mitre.org if you require further clarification on this issue.

   LICENSE

   CWE Submissions: By submitting materials to The MITRE Corporation’s ("MITRE") Common Weakness Enumeration Program (CWE™), you hereby grant to MITRE a perpetual, worldwide, non-exclusive, no-charge, royalty-free, irrevocable copyright license to use, reproduce, prepare derivative works of, publicly display, publicly perform, sublicense, and distribute your submitted materials and derivative works. Unless otherwise required by applicable law or agreed to in writing, it is understood that you are providing such materials on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied, including, without limitation, any warranties or conditions of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A PARTICULAR PURPOSE.

   CWE Usage: MITRE hereby grants you a non-exclusive, royalty-free license to use CWE for research, development, and commercial purposes. Any copy you make for such purposes is authorized on the condition that you reproduce MITRE’s copyright designation and this license in any such copy.

   DISCLAIMERS

   ALL DOCUMENTS AND THE INFORMATION CONTAINED IN THE CWE ARE PROVIDED ON AN "AS IS" BASIS AND THE CONTRIBUTOR, THE ORGANIZATION HE/SHE REPRESENTS OR IS SPONSORED BY (IF ANY), THE MITRE CORPORATION, ITS BOARD OF TRUSTEES, OFFICERS, AGENTS, AND EMPLOYEES, DISCLAIM ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY WARRANTY THAT THE USE OF THE INFORMATION THEREIN WILL NOT INFRINGE ANY RIGHTS OR ANY IMPLIED WARRANTIES OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.

   IN NO EVENT SHALL THE CONTRIBUTOR, THE ORGANIZATION HE/SHE REPRESENTS OR IS SPONSORED BY (IF ANY), THE MITRE CORPORATION, ITS BOARD OF TRUSTEES, OFFICERS, AGENTS, AND EMPLOYEES BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE INFORMATION OR THE USE OR OTHER DEALINGS IN THE CWE.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Support decls for example 1.  */

extern unsigned int GetUntrustedSizeValue();
extern void ExitError(const char *) __attribute__((noreturn));

typedef struct Widget
{
} Widget;

#define MAX_NUM_WIDGETS 100

extern Widget *InitializeWidget();
extern void showWidgets(Widget **);

void example_1 (void)
{
  int i;
  unsigned int numWidgets;
  Widget **WidgetList;

  numWidgets = GetUntrustedSizeValue();
  if ((numWidgets == 0) || (numWidgets > MAX_NUM_WIDGETS)) {
    ExitError("Incorrect number of widgets requested!");
  }
  WidgetList = (Widget **)malloc(numWidgets * sizeof(Widget *));
  printf("WidgetList ptr=%p\n", WidgetList);
  for(i=0; i<numWidgets; i++) {
    WidgetList[i] = InitializeWidget(); /* { dg-warning "dereference of possibly-NULL 'WidgetList'" } */
  }
  WidgetList[numWidgets] = NULL; /* { dg-warning "heap-based buffer overflow" } */
  showWidgets(WidgetList);
}

/* Support decls for example 2.  */

typedef struct img_t
{
  char placeholder[1024];
} img_t;

extern int get_num_imgs();

img_t *example_2 (void)
{
  img_t *table_ptr; /*struct containing img data, 10kB each*/
  int num_imgs;
  /* ... */
  num_imgs = get_num_imgs();
  table_ptr = (img_t*)malloc(sizeof(img_t)*num_imgs); /* TODO: ideally we'd warn about possible overflow here.  */
  /* ... */
  return table_ptr;
}

/* Support decls for example 3.  */

#define MAX_SIZE 100
extern void die(const char *) __attribute__((noreturn));

char * example_3 (char *user_supplied_string)
{
  int i, dst_index;
  char *dst_buf = (char*)malloc(4*sizeof(char) * MAX_SIZE);
  if ( MAX_SIZE <= strlen(user_supplied_string) ){
    die("user string too long, die evil hacker!");
  }
  dst_index = 0;
  for ( i = 0; i < strlen(user_supplied_string); i++ ){
    if( '&' == user_supplied_string[i] ){
      dst_buf[dst_index++] = '&'; /* { dg-warning "dereference of possibly-NULL 'dst_buf'" } */
      dst_buf[dst_index++] = 'a';
      dst_buf[dst_index++] = 'm';
      dst_buf[dst_index++] = 'p';
      dst_buf[dst_index++] = ';'; /* TODO: ideally we'd warn about possible out-of-bounds write here.  */
    }
    else if ('<' == user_supplied_string[i] ){
      /* encode to &lt; */
    }
    else dst_buf[dst_index++] = user_supplied_string[i]; /* { dg-warning "dereference of possibly-NULL 'dst_buf'" } */
  }
  return dst_buf;
}

/* Support decls for example 4.  */

typedef struct DataPacket { int headers; } DataPacket;
typedef struct PacketHeader {} PacketHeader;
extern int AcceptSocketConnection();
extern void ReadPacket(DataPacket *, int);
extern void ParsePacketHeaders(DataPacket *, PacketHeader *);

void example_4 (DataPacket *packet)
{
  int sock;

  int numHeaders;
  PacketHeader *headers;

  sock=AcceptSocketConnection();
  ReadPacket(packet, sock);
  numHeaders =packet->headers;

  if (numHeaders > 100) {
    ExitError("too many headers!");
  }
  headers = malloc(numHeaders * sizeof(PacketHeader)); /* TODO: ideally we'd warn about possible overflow here with negative numHeaders.  */
  ParsePacketHeaders(packet, headers);
}

void example_5 (void)
{
  int *id_sequence;

  /* Allocate space for an array of three ids. */
  id_sequence = (int*) malloc(3); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
  if (id_sequence == NULL) exit(1);

  /* Populate the id array. */
  id_sequence[0] = 13579; /* { dg-warning "heap-based buffer overflow" } */
  id_sequence[1] = 24680; /* { dg-warning "heap-based buffer overflow" } */
  id_sequence[2] = 97531; /* { dg-warning "heap-based buffer overflow" } */
} /* { dg-warning "leak of 'id_sequence'" } */
