// Build don't link: 
// GROUPS passed parsing
// parsing folder
// From: szahn%Robinie@goesser.sie.siemens.co.at (Hr. Zahn)
// Date:     Mon, 5 Jul 93 10:45:51 +0200
// Subject:  Bug report g++ 2.4.5, unexpected syntax errors
// Message-ID: <9307050845.AA00499@ets5.uebemc.siemens.de>

int f1(
    int (**a1)()
    );

int f2(
    int (**a1)()
    );


int f3(
    int (**a1)( int a, int b )
    );

int f4(
    int (**a1)( int a, int b )
    );
