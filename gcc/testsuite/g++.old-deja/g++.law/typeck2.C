// Build don't link: 
// GROUPS passed typeck
// typeck file
// From: Jutta Degener <jutta@cs.tu-berlin.de>
// Date:     Wed, 9 Jun 1993 17:58:35 +0200 (MET DST)
// Subject:  2.4.3: Type of new <typedef'ed array>
// Message-ID: <199306091558.AA19075@mail.cs.tu-berlin.de>

  typedef int arr[10];
int main()
  {
        int * p = new int[10];
        int * q = new arr;              /* g++ complains, but shouldn't     */
        int (* r)[10] = new arr;        /* g++ doesn't complain, but should */// ERROR - 
  }

