/* Example heavily adapted from https://cwe.mitre.org/data/definitions/469.html
   which states "Copyright © 2006–2024, The MITRE Corporation. CWE, CWSS, CWRAF, and the CWE logo are trademarks of The MITRE Corporation."
   and which has this on:
     https://cwe.mitre.org/about/termsofuse.html

     Terms of Use

     CWE™ is free to use by any organization or individual for any research, development, and/or commercial purposes, per these CWE Terms of Use. Accordingly, The MITRE Corporation hereby grants you a non-exclusive, royalty-free license to use CWE for research, development, and commercial purposes. Any copy you make for such purposes is authorized on the condition that you reproduce MITRE’s copyright designation and this license in any such copy. CWE is a trademark of The MITRE Corporation. Please contact cwe@mitre.org if you require further clarification on this issue.

     DISCLAIMERS

     By accessing information through this site you (as “the user”) hereby agrees the site and the information is provided on an “as is” basis only without warranty of any kind, express or implied, including but not limited to implied warranties of merchantability, availability, accuracy, noninfringement, or fitness for a particular purpose. Use of this site and the information is at the user’s own risk. The user shall comply with all applicable laws, rules, and regulations, and the data source’s restrictions, when using the site.

     By contributing information to this site you (as “the contributor”) hereby represents and warrants the contributor has obtained all necessary permissions from copyright holders and other third parties to allow the contributor to contribute, and this site to host and display, the information and any such contribution, hosting, and displaying will not violate any law, rule, or regulation. Additionally, the contributor hereby grants all users of such information a perpetual, worldwide, non-exclusive, no-charge, royalty-free, irrevocable license to reproduce, prepare derivative works of, publicly display, publicly perform, sublicense, and distribute such information and all derivative works.

     The MITRE Corporation expressly disclaims any liability for any damages arising from the contributor’s contribution of such information, the user’s use of the site or such information, and The MITRE Corporation’s hosting the tool and displaying the information. The foregoing disclaimer specifically includes but is not limited to general, consequential, indirect, incidental, exemplary, or special or punitive damages (including but not limited to loss of income, program interruption, loss of information, or other pecuniary loss) arising out of use of this information, no matter the cause of action, even if The MITRE Corporation has been advised of the possibility of such damages.  */

/* We need this for the issue to be found, or the loop analysis
   is too simplistic.  */
/* { dg-additional-options "-fno-analyzer-state-merge" } */
/* { dg-additional-options "-Wno-analyzer-too-complex" } */
/* { dg-additional-options "-Wno-analyzer-symbol-too-complex" } */

#include <stddef.h>

struct node {
  int data;
  struct node* next;
};

/* The example fails to initialize "tail" for the head == NULL case.  */

int example_1_with_uninit (struct node* head) {
  struct node* current = head;
  struct node* tail;
  while (current != NULL) {
    tail = current;
    current = current->next;
  }
  return tail - head; /* { dg-warning "use of uninitialized value 'tail'" } */
}

int example_1_bad (struct node* head) {
  struct node* current = head;
  struct node* tail = head; /* initialization added */
  while (current != NULL) {
    tail = current;
    current = current->next;
  }
  return tail - head; /* { dg-warning "undefined behavior when subtracting pointers" } */
}

int example_1_good (struct node* head) {
  struct node* current = head;
  int count = 0;
  while (current != NULL) {
    count++;
    current = current->next;
  }
  return count;
}

/* We need to add usage of the function to detect the issue.  */

int usage_of_example_1_bad (void)
{
  struct node p; /* { dg-message "right-hand side" } */
  struct node q; /* { dg-message "left-hand side" } */
  p.next = &q;
  q.next = NULL;
  return example_1_bad (&p);
}

int usage_of_example_1_good (void)
{
  struct node p;
  struct node q;
  p.next = &q;
  q.next = NULL;
  return example_1_good (&p);
}
