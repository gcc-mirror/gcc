/* { dg-do compile } */
/* { dg-options "-O2 -Wformat-overflow=1" } */

void acpi_gpiochip_request_interrupt(unsigned short s)
{
        char name[3];
	unsigned int pin = s;

	if (pin <= 255)
		__builtin_sprintf(name, "%02X", pin);
}

