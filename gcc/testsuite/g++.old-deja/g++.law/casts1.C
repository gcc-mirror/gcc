// Build don't link: 
// GROUPS passed casts
// casts file
// From: dcb@us-es.sel.de (David Binderman 3841)
// Date:     Thu, 18 Feb 93 14:42:48 +0100
// Subject:  Page 67 of the ARM
// Message-ID: <9302181342.AA14050@slsvitt>

int main() {
   (struct T { int b; } *) 0;      // ERROR - 

        return 0;
}
