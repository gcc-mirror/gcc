// { dg-do assemble  }
// GROUPS passed ARM-compliance
// arm file
// Message-Id: <9302181055.AA12522@slsvitt>
// From: dcb@us-es.sel.de (David Binderman 3841)
// Subject: Page 81 of the ARM
// Date: Thu, 18 Feb 93 11:55:15 +0100


int main() {
        int x[ 10, 11]; // { dg-error "" } .*

        return 0;
}

